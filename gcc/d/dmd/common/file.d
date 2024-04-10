/**
 * File utilities.
 *
 * Functions and objects dedicated to file I/O and management. TODO: Move here artifacts
 * from places such as root/ so both the frontend and the backend have access to them.
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/common/file.d, common/_file.d)
 * Documentation: https://dlang.org/phobos/dmd_common_file.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/common/file.d
 */

module dmd.common.file;

import core.stdc.errno : errno;
import core.stdc.stdio : fprintf, remove, rename, stderr;
import core.stdc.stdlib;
import core.stdc.string : strerror, strlen, memcpy;

import dmd.common.smallbuffer;

version (Windows)
{
    import core.sys.windows.winbase;
    import core.sys.windows.winnls : CP_ACP;
    import core.sys.windows.winnt;

    enum CodePage = CP_ACP; // assume filenames encoded in system default Windows ANSI code page
    enum invalidHandle = INVALID_HANDLE_VALUE;
}
else version (Posix)
{
    import core.sys.posix.fcntl;
    import core.sys.posix.sys.mman;
    import core.sys.posix.sys.stat;
    import core.sys.posix.unistd;
    import core.sys.posix.utime;

    enum invalidHandle = -1;
}
else
    static assert(0);




nothrow:

/**
Encapsulated management of a memory-mapped file.

Params:
Datum = the mapped data type: Use a POD of size 1 for read/write mapping
and a `const` version thereof for read-only mapping. Other primitive types
should work, but have not been yet tested.
*/
struct FileMapping(Datum)
{
    static assert(__traits(isPOD, Datum) && Datum.sizeof == 1,
        "Not tested with other data types yet. Add new types with care.");

    // state {
    /// Handle of underlying file
    private auto handle = invalidHandle;
    /// File mapping object needed on Windows
    version(Windows) private HANDLE fileMappingObject = invalidHandle;
    /// Memory-mapped array
    private Datum[] data;
    /// Name of underlying file, zero-terminated
    private const(char)* name;
    // state }

  nothrow:

    /**
    Open `filename` and map it in memory. If `Datum` is `const`, opens for
    read-only and maps the content in memory; no error is issued if the file
    does not exist. This makes it easy to treat a non-existing file as empty.

    If `Datum` is mutable, opens for read/write (creates file if it does not
    exist) and fails fatally on any error.

    Due to quirks in `mmap`, if the file is empty, `handle` is valid but `data`
    is `null`. This state is valid and accounted for.

    Params:
    filename = the name of the file to be mapped in memory
    */
    this(const char* filename)
    {
        version (Posix)
        {
            handle = open(filename, is(Datum == const) ? O_RDONLY : (O_CREAT | O_RDWR),
                S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);

            if (handle == invalidHandle)
            {
                static if (is(Datum == const))
                {
                    // No error, nonexisting file in read mode behaves like an empty file.
                    return;
                }
                else
                {
                    fprintf(stderr, "open(\"%s\") failed: %s\n", filename, strerror(errno));
                    exit(1);
                }
            }

            const size = fileSize(handle);

            if (size > 0 && size != ulong.max && size <= size_t.max)
            {
                auto p = mmap(null, cast(size_t) size, is(Datum == const) ? PROT_READ : PROT_WRITE, MAP_SHARED, handle, 0);
                if (p == MAP_FAILED)
                {
                    fprintf(stderr, "mmap(null, %zu) for \"%s\" failed: %s\n", cast(size_t) size, filename, strerror(errno));
                    exit(1);
                }
                // The cast below will always work because it's gated by the `size <= size_t.max` condition.
                data = cast(Datum[]) p[0 .. cast(size_t) size];
            }
        }
        else version(Windows)
        {
            static if (is(Datum == const))
            {
                enum createFileMode = GENERIC_READ;
                enum openFlags = OPEN_EXISTING;
            }
            else
            {
                enum createFileMode = GENERIC_READ | GENERIC_WRITE;
                enum openFlags = CREATE_ALWAYS;
            }

            handle = filename[0 .. strlen(filename)].
                extendedPathThen!(p => CreateFileW(p.ptr, createFileMode, 0, null, openFlags, FILE_ATTRIBUTE_NORMAL, null));
            if (handle == invalidHandle)
            {
                static if (is(Datum == const))
                {
                    return;
                }
                else
                {
                    fprintf(stderr, "CreateFileW() failed for \"%s\": %d\n", filename, GetLastError());
                    exit(1);
                }
            }
            createMapping(filename, fileSize(handle));
        }
        else static assert(0);

        // Save the name for later. Technically there's no need: on Linux one can use readlink on /proc/self/fd/NNN.
        // On BSD and OSX one can use fcntl with F_GETPATH. On Windows one can use GetFileInformationByHandleEx.
        // But just saving the name is simplest, fastest, and most portable...
        const totalNameLength = filename.strlen() + 1;
        auto namex = cast(char*) malloc(totalNameLength);
        if (!namex)
        {
            fprintf(stderr, "FileMapping: Out of memory.");
            exit(1);
        }
        name = cast(char*) memcpy(namex, filename, totalNameLength);
    }

    /**
    Common code factored opportunistically. Windows only. Assumes `handle` is
    already pointing to an opened file. Initializes the `fileMappingObject`
    and `data` members.

    Params:
    filename = the file to be mapped
    size = the size of the file in bytes
    */
    version(Windows) private void createMapping(const char* filename, ulong size)
    {
        assert(size <= size_t.max || size == ulong.max);
        assert(handle != invalidHandle);
        assert(data is null);
        assert(fileMappingObject == invalidHandle);

        if (size == 0 || size == ulong.max)
            return;

        static if (is(Datum == const))
        {
            enum fileMappingFlags = PAGE_READONLY;
            enum mapViewFlags = FILE_MAP_READ;
        }
        else
        {
            enum fileMappingFlags = PAGE_READWRITE;
            enum mapViewFlags = FILE_MAP_WRITE;
        }

        fileMappingObject = CreateFileMappingW(handle, null, fileMappingFlags, 0, 0, null);
        if (!fileMappingObject)
        {
            fprintf(stderr, "CreateFileMappingW(%p) failed for %llu bytes of \"%s\": %d\n",
                handle, size, filename, GetLastError());
            fileMappingObject = invalidHandle;  // by convention always use invalidHandle, not null
            exit(1);
        }
        auto p = MapViewOfFile(fileMappingObject, mapViewFlags, 0, 0, 0);
        if (!p)
        {
            fprintf(stderr, "MapViewOfFile() failed for \"%s\": %d\n", filename, GetLastError());
            exit(1);
        }
        data = cast(Datum[]) p[0 .. cast(size_t) size];
    }

    // Not copyable or assignable (for now).
    @disable this(const FileMapping!Datum rhs);
    @disable void opAssign(const ref FileMapping!Datum rhs);

    /**
    Frees resources associated with this mapping. However, it does not deallocate the name.
    */
    ~this() pure nothrow
    {
        if (!active)
            return;
        fakePure({
            version (Posix)
            {
                // Cannot call fprintf from inside a destructor, so exiting silently.

                if (data.ptr && munmap(cast(void*) data.ptr, data.length) != 0)
                {
                    exit(1);
                }
                data = null;
                if (handle != invalidHandle && .close(handle) != 0)
                {
                    exit(1);
                }
                handle = invalidHandle;
            }
            else version(Windows)
            {
                if (data.ptr !is null && UnmapViewOfFile(cast(void*) data.ptr) == 0)
                {
                    exit(1);
                }
                data = null;
                if (fileMappingObject != invalidHandle && CloseHandle(fileMappingObject) == 0)
                {
                    exit(1);
                }
                fileMappingObject = invalidHandle;
                if (handle != invalidHandle && CloseHandle(handle) == 0)
                {
                    exit(1);
                }
                handle = invalidHandle;
            }
            else static assert(0);
        });
    }

    /**
    Returns the zero-terminated file name associated with the mapping. Can NOT
    be saved beyond the lifetime of `this`.
    */
    private const(char)* filename() const pure @nogc @safe nothrow { return name; }

    /**
    Frees resources associated with this mapping. However, it does not deallocate the name.
    Reinitializes `this` as a fresh object that can be reused.
    */
    void close()
    {
        __dtor();
        handle = invalidHandle;
        version(Windows) fileMappingObject = invalidHandle;
        data = null;
        name = null;
    }

    /**
    Deletes the underlying file and frees all resources associated.
    Reinitializes `this` as a fresh object that can be reused.

    This function does not abort if the file cannot be deleted, but does print
    a message on `stderr` and returns `false` to the caller. The underlying
    rationale is to give the caller the option to continue execution if
    deleting the file is not important.

    Returns: `true` iff the file was successfully deleted. If the file was not
    deleted, prints a message to `stderr` and returns `false`.
    */
    static if (!is(Datum == const))
    bool discard()
    {
        // Truncate file to zero so unflushed buffers are not flushed unnecessarily.
        resize(0);
        auto deleteme = name;
        close();
        // In-memory resource freed, now get rid of the underlying temp file.
        version(Posix)
        {
            if (unlink(deleteme) != 0)
            {
                fprintf(stderr, "unlink(\"%s\") failed: %s\n", filename, strerror(errno));
                return false;
            }
        }
        else version(Windows)
        {
            if (deleteme[0 .. strlen(deleteme)].extendedPathThen!(p => DeleteFileW(p.ptr)) == 0)
            {
                fprintf(stderr, "DeleteFileW error %d\n", GetLastError());
                return false;
            }
        }
        else static assert(0);
        return true;
    }

    /**
    Queries whether `this` is currently associated with a file.

    Returns: `true` iff there is an active mapping.
    */
    bool active() const pure @nogc nothrow
    {
        return handle !is invalidHandle;
    }

    /**
    Queries the length of the file associated with this mapping.  If not
    active, returns 0.

    Returns: the length of the file, or 0 if no file associated.
    */
    size_t length() const pure @nogc @safe nothrow { return data.length; }

    /**
    Get a slice to the contents of the entire file.

    Returns: the contents of the file. If not active, returns the `null` slice.
    */
    auto opSlice() pure @nogc @safe nothrow { return data; }

    /**
    Resizes the file and mapping to the specified `size`.

    Params:
    size = new length requested
    */
    static if (!is(Datum == const))
    void resize(size_t size) pure
    {
        assert(handle != invalidHandle);
        fakePure({
            version(Posix)
            {
                if (data.length)
                {
                    assert(data.ptr, "Corrupt memory mapping");
                    // assert(0) here because it would indicate an internal error
                    munmap(cast(void*) data.ptr, data.length) == 0 || assert(0);
                    data = null;
                }
                if (ftruncate(handle, size) != 0)
                {
                    fprintf(stderr, "ftruncate() failed for \"%s\": %s\n", filename, strerror(errno));
                    exit(1);
                }
                if (size > 0)
                {
                    auto p = mmap(null, size, PROT_WRITE, MAP_SHARED, handle, 0);
                    if (cast(ssize_t) p == -1)
                    {
                        fprintf(stderr, "mmap() failed for \"%s\": %s\n", filename, strerror(errno));
                        exit(1);
                    }
                    data = cast(Datum[]) p[0 .. size];
                }
            }
            else version(Windows)
            {
                // Per documentation, must unmap first.
                if (data.length > 0 && UnmapViewOfFile(cast(void*) data.ptr) == 0)
                {
                    fprintf(stderr, "UnmapViewOfFile(%p) failed for memory mapping of \"%s\": %d\n",
                        data.ptr, filename, GetLastError());
                    exit(1);
                }
                data = null;
                if (fileMappingObject != invalidHandle && CloseHandle(fileMappingObject) == 0)
                {
                    fprintf(stderr, "CloseHandle() failed for memory mapping of \"%s\": %d\n", filename, GetLastError());
                    exit(1);
                }
                fileMappingObject = invalidHandle;
                LARGE_INTEGER biggie;
                biggie.QuadPart = size;
                if (SetFilePointerEx(handle, biggie, null, FILE_BEGIN) == 0 || SetEndOfFile(handle) == 0)
                {
                    fprintf(stderr, "SetFilePointer() failed for \"%s\": %d\n", filename, GetLastError());
                    exit(1);
                }
                createMapping(name, size);
            }
            else static assert(0);
        });
    }

    /**
    Unconditionally and destructively moves the underlying file to `filename`.
    If the operation succeeds, returns true. Upon failure, prints a message to
    `stderr` and returns `false`. In all cases it closes the underlying file.

    Params: filename = zero-terminated name of the file to move to.

    Returns: `true` iff the operation was successful.
    */
    bool moveToFile(const char* filename)
    {
        assert(name !is null);

        // Fetch the name and then set it to `null` so it doesn't get deallocated
        auto oldname = name;
        scope(exit) free(cast(void*) oldname);
        name = null;
        close();

        // Rename the underlying file to the target, no copy necessary.
        version(Posix)
        {
            if (.rename(oldname, filename) != 0)
            {
                fprintf(stderr, "rename(\"%s\", \"%s\") failed: %s\n", oldname, filename, strerror(errno));
                return false;
            }
        }
        else version(Windows)
        {
            auto r = oldname[0 .. strlen(oldname)].extendedPathThen!(
                p1 => filename[0 .. strlen(filename)].extendedPathThen!(p2 => MoveFileExW(p1.ptr, p2.ptr, MOVEFILE_REPLACE_EXISTING))
            );
            if (r == 0)
            {
                fprintf(stderr, "MoveFileExW(\"%s\", \"%s\") failed: %d\n", oldname, filename, GetLastError());
                return false;
            }
        }
        else static assert(0);
        return true;
    }
}

/// Write a file, returning `true` on success.
extern(D) static bool writeFile(const(char)* name, const void[] data) nothrow
{
    version (Posix)
    {
        int fd = open(name, O_CREAT | O_WRONLY | O_TRUNC, (6 << 6) | (4 << 3) | 4);
        if (fd == -1)
            goto err;
        if (.write(fd, data.ptr, data.length) != data.length)
            goto err2;
        if (close(fd) == -1)
            goto err;
        return true;
    err2:
        close(fd);
        .remove(name);
    err:
        return false;
    }
    else version (Windows)
    {
        DWORD numwritten; // here because of the gotos
        const nameStr = name[0 .. strlen(name)];
        // work around Windows file path length limitation
        // (see documentation for extendedPathThen).
        HANDLE h = nameStr.extendedPathThen!
            (p => CreateFileW(p.ptr,
                                GENERIC_WRITE,
                                0,
                                null,
                                CREATE_ALWAYS,
                                FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,
                                null));
        if (h == INVALID_HANDLE_VALUE)
            goto err;

        if (WriteFile(h, data.ptr, cast(DWORD)data.length, &numwritten, null) != TRUE)
            goto err2;
        if (numwritten != data.length)
            goto err2;
        if (!CloseHandle(h))
            goto err;
        return true;
    err2:
        CloseHandle(h);
        nameStr.extendedPathThen!(p => DeleteFileW(p.ptr));
    err:
        return false;
    }
    else
    {
        static assert(0);
    }
}

/// Touch a file to current date
bool touchFile(const char* namez)
{
    version (Windows)
    {
        FILETIME ft = void;
        SYSTEMTIME st = void;
        GetSystemTime(&st);
        SystemTimeToFileTime(&st, &ft);

        // get handle to file
        HANDLE h = namez[0 .. namez.strlen()].extendedPathThen!(p => CreateFile(p.ptr,
            FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ | FILE_SHARE_WRITE,
            null, OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL, null));
        if (h == INVALID_HANDLE_VALUE)
            return false;

        const f = SetFileTime(h, null, null, &ft); // set last write time

        if (!CloseHandle(h))
            return false;

        return f != 0;
    }
    else version (Posix)
    {
        return utime(namez, null) == 0;
    }
    else
        static assert(0);
}

// Feel free to make these public if used elsewhere.
/**
Size of a file in bytes.
Params: fd = file handle
Returns: file size in bytes, or `ulong.max` on any error.
*/
version (Posix)
{
    private ulong fileSize(int fd)
    {
        stat_t buf;
        if (fstat(fd, &buf) == 0)
            return buf.st_size;
        return ulong.max;
    }
}
else version (Windows)
{
    /// Ditto
    private ulong fileSize(HANDLE fd)
    {
        ulong result;
        if (GetFileSizeEx(fd, cast(LARGE_INTEGER*) &result) == 0)
            return result;
        return ulong.max;
    }
}
else
    static assert(0);

/**
Runs a non-pure function or delegate as pure code. Use with caution.

Params:
fun = the delegate to run, usually inlined: `fakePure({ ... });`

Returns: whatever `fun` returns.
*/
private auto ref fakePure(F)(scope F fun) pure
{
    mixin("alias PureFun = " ~ F.stringof ~ " pure;");
    return (cast(PureFun) fun)();
}
