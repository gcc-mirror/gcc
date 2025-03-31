/**
 * Read a file from disk and store it in memory.
 *
 * Copyright: Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/root/file.d, root/_file.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_file.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/root/file.d
 */

module dmd.root.file;

import core.stdc.errno;
import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string : strerror;
import core.sys.posix.fcntl;
import core.sys.posix.unistd;
import core.sys.windows.winbase;
import core.sys.windows.winnt;

import dmd.root.filename;
import dmd.root.rmem;
import dmd.root.string;

import dmd.common.file;
import dmd.common.outbuffer;
import dmd.common.smallbuffer;

nothrow:

/// Owns a (rmem-managed) buffer.
struct Buffer
{
    ubyte[] data;

  nothrow:

    this(this) @disable;

    ~this() pure nothrow
    {
        mem.xfree(data.ptr);
    }

    /// Transfers ownership of the buffer to the caller.
    ubyte[] extractSlice() pure nothrow @nogc @safe
    {
        auto result = data;
        data = null;
        return result;
    }
}

///
struct File
{
    ///
    static struct ReadResult
    {
        bool success;
        Buffer buffer;

        /// Transfers ownership of the buffer to the caller.
        ubyte[] extractSlice() pure nothrow @nogc @safe
        {
            return buffer.extractSlice();
        }

        /// ditto
        /// Include the null-terminator at the end of the buffer in the returned array.
        ubyte[] extractDataZ() @nogc nothrow pure
        {
            auto result = buffer.extractSlice();
            return result.ptr[0 .. result.length + 1];
        }
    }

nothrow:
    /** Read the full content of a file, and append it to `buffer`
     * Params:
     *  name = name of file
     *  buffer = file contents appended to it
     * Returns:
     *  false = success, true = failed
     */
    static bool read(const char[] name, ref OutBuffer buffer)
    {
        enum Success = false;
        enum Failure = true;

        version (Posix)
        {
            //printf("File::read('%.*s')\n", cast(int)name.length, name.ptr);
            int fd = name.toCStringThen!(slice => open(slice.ptr, O_RDONLY));
            if (fd == -1)
            {
                //perror("\topen error");
                return Failure;
            }
            //printf("\tfile opened\n");
            stat_t statbuf;
            if (fstat(fd, &statbuf))
            {
                //perror("\tfstat error");
                close(fd);
                return Failure;
            }
            size_t size = cast(size_t)statbuf.st_size;
            auto buf = buffer.allocate(size);
            ssize_t numread = .read(fd, buf.ptr, size);
            if (numread != size)
            {
                //perror("\tread error");
                close(fd);
                return Failure;
            }
            if (close(fd) == -1)
            {
                //perror("\tclose error");
                return Failure;
            }
        }
        else version (Windows)
        {
            // work around Windows file path length limitation
            // (see documentation for extendedPathThen).
            HANDLE h = name.extendedPathThen!
                (p => CreateFileW(p.ptr,
                                  GENERIC_READ,
                                  FILE_SHARE_READ,
                                  null,
                                  OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,
                                  null));
            if (h == INVALID_HANDLE_VALUE)
                return Failure;
            DWORD size = GetFileSize(h, null);
            auto buf = buffer.allocate(size);
            DWORD numread;
            if (ReadFile(h, buf.ptr, size, &numread, null) != TRUE ||
                numread != size)
            {
                CloseHandle(h);
                return Failure;
            }
            if (!CloseHandle(h))
                return Failure;
        }
        else
        {
            static assert(0);
        }
        return Success;
    }

    /// Write a file, returning `true` on success.
    static bool write(const(char)* name, const void[] data)
    {
        import dmd.common.file : writeFile;
        return writeFile(name, data);
    }

    ///ditto
    static bool write(const(char)[] name, const void[] data)
    {
        return name.toCStringThen!((fname) => write(fname.ptr, data));
    }

    /// Delete a file.
    extern (C++) static void remove(const(char)* name)
    {
        version (Posix)
        {
            .remove(name);
        }
        else version (Windows)
        {
            name.toDString.extendedPathThen!(p => DeleteFileW(p.ptr));
        }
        else
        {
            static assert(0);
        }
    }

    /***************************************************
     * Update file
     *
     * If the file exists and is identical to what is to be written,
     * merely update the timestamp on the file.
     * Otherwise, write the file.
     *
     * The idea is writes are much slower than reads, and build systems
     * often wind up generating identical files.
     * Params:
     *  name = name of file to update
     *  data = updated contents of file
     * Returns:
     *  `true` on success
     */
    static bool update(const(char)* namez, const void[] data)
    {
        enum log = false;
        if (log) printf("update %s\n", namez);

        if (data.length != File.size(namez))
            return write(namez, data);               // write new file

        if (log) printf("same size\n");

        /* The file already exists, and is the same size.
         * Read it in, and compare for equality.
         */
        //if (FileMapping!(const ubyte)(namez)[] != data[])
            return write(namez, data); // contents not same, so write new file
        //if (log) printf("same contents\n");

        /* Contents are identical, so set timestamp of existing file to current time
         */
        //return touch(namez);
    }

    ///ditto
    static bool update(const(char)[] name, const void[] data)
    {
        return name.toCStringThen!(fname => update(fname.ptr, data));
    }

    /// Size of a file in bytes.
    /// Params: namez = null-terminated filename
    /// Returns: `ulong.max` on any error, the length otherwise.
    static ulong size(const char* namez)
    {
        version (Posix)
        {
            stat_t buf;
            if (stat(namez, &buf) == 0)
                return buf.st_size;
        }
        else version (Windows)
        {
            const nameStr = namez.toDString();
            import core.sys.windows.windows;
            WIN32_FILE_ATTRIBUTE_DATA fad = void;
            // Doesn't exist, not a regular file, different size
            if (nameStr.extendedPathThen!(p => GetFileAttributesExW(p.ptr, GET_FILEEX_INFO_LEVELS.GetFileExInfoStandard, &fad)) != 0)
                return (ulong(fad.nFileSizeHigh) << 32UL) | fad.nFileSizeLow;
        }
        else static assert(0);
        // Error cases go here.
        return ulong.max;
    }
}

private
{
    version (linux) version (PPC)
    {
        // https://issues.dlang.org/show_bug.cgi?id=22823
        // Define our own version of stat_t, as older versions of the compiler
        // had the st_size field at the wrong offset on PPC.
        alias stat_t_imported = core.sys.posix.sys.stat.stat_t;
        static if (stat_t_imported.st_size.offsetof != 48)
        {
            extern (C) nothrow @nogc:
            struct stat_t
            {
                ulong[6] __pad1;
                ulong st_size;
                ulong[6] __pad2;
            }
            version (CRuntime_Glibc)
            {
                int fstat64(int, stat_t*) @trusted;
                alias fstat = fstat64;
                int stat64(const scope char*, stat_t*) @system;
                alias stat = stat64;
            }
            else
            {
                int fstat(int, stat_t*) @trusted;
                int stat(const scope char*, stat_t*) @system;
            }
        }
    }
}
