/**
 * Encapsulate path and file names.
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/filename.d, root/_filename.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_filename.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/filename.d
 */

module dmd.root.filename;

import core.stdc.ctype;
import core.stdc.errno;
import core.stdc.string;

import dmd.common.file;
import dmd.common.outbuffer;

import dmd.root.array;
import dmd.root.file;
import dmd.root.port;
import dmd.root.rmem;
import dmd.root.string;

version (Posix)
{
    import core.sys.posix.stdlib;
    import core.sys.posix.sys.stat;
    import core.sys.posix.unistd : getcwd;
}

version (Windows)
{
    import core.sys.windows.winbase;
    import core.sys.windows.windef;
    import core.sys.windows.winnls;

    import dmd.common.smallbuffer : extendedPathThen;

    extern (Windows) DWORD GetFullPathNameW(LPCWSTR, DWORD, LPWSTR, LPWSTR*) nothrow @nogc;
    extern (Windows) void SetLastError(DWORD) nothrow @nogc;
    extern (C) char* getcwd(char* buffer, size_t maxlen) nothrow;
}

version (CRuntime_Glibc)
{
    extern (C) char* canonicalize_file_name(const char*) nothrow;
}

alias Strings = Array!(const(char)*);


// Check whether character is a directory separator
bool isDirSeparator(char c) pure nothrow @nogc @safe
{
    version (Windows)
    {
        return c == '\\' || c == '/';
    }
    else version (Posix)
    {
        return c == '/';
    }
    else
    {
        assert(0);
    }
}

/***********************************************************
 * Encapsulate path and file names.
 */
struct FileName
{
nothrow:
    private const(char)[] str;

    ///
    extern (D) this(const char[] str) pure
    {
        this.str = str.xarraydup;
    }

    ///
    extern (C++) static FileName create(const(char)* name) pure
    {
        return FileName(name.toDString);
    }

    /// Compare two name according to the platform's rules (case sensitive or not)
    extern (C++) static bool equals(const(char)* name1, const(char)* name2) pure @nogc
    {
        return equals(name1.toDString, name2.toDString);
    }

    /// Ditto
    extern (D) static bool equals(const char[] name1, const char[] name2) pure @nogc
    {
        if (name1.length != name2.length)
            return false;

        version (Windows)
        {
            return name1.ptr == name2.ptr ||
                   Port.memicmp(name1.ptr, name2.ptr, name1.length) == 0;
        }
        else
        {
            return name1 == name2;
        }
    }

    /************************************
     * Determine if path is absolute.
     * Params:
     *  name = path
     * Returns:
     *  true if absolute path name.
     */
    extern (C++) static bool absolute(const(char)* name) pure @nogc
    {
        return absolute(name.toDString);
    }

    /// Ditto
    extern (D) static bool absolute(const char[] name) pure @nogc @safe
    {
        if (!name.length)
            return false;

        version (Windows)
        {
            return isDirSeparator(name[0])
                || (name.length >= 2 && name[1] == ':');
        }
        else version (Posix)
        {
            return isDirSeparator(name[0]);
        }
        else
        {
            assert(0);
        }
    }

    unittest
    {
        assert(absolute("/"[]) == true);
        assert(absolute(""[]) == false);

        version (Windows)
        {
            assert(absolute(r"\"[]) == true);
            assert(absolute(r"\\"[]) == true);
            assert(absolute(r"c:"[]) == true);
        }
    }

    /**
    Return the given name as an absolute path

    Params:
        name = path
        base = the absolute base to prefix name with if it is relative

    Returns: name as an absolute path relative to base
    */
    extern (C++) static const(char)* toAbsolute(const(char)* name, const(char)* base = null)
    {
        const name_ = name.toDString();
        const base_ = base ? base.toDString() : getcwd(null, 0).toDString();
        return absolute(name_) ? name : combine(base_, name_).ptr;
    }

    /********************************
     * Determine file name extension as slice of input.
     * Params:
     *  str = file name
     * Returns:
     *  filename extension (read-only).
     *  Points past '.' of extension.
     *  If there isn't one, return null.
     */
    extern (C++) static const(char)* ext(const(char)* str) pure @nogc
    {
        return ext(str.toDString).ptr;
    }

    /// Ditto
    extern (D) static const(char)[] ext(const char[] str) nothrow pure @safe @nogc
    {
        foreach_reverse (idx, char e; str)
        {
            switch (e)
            {
            case '.':
                return str[idx + 1 .. $];
            version (Posix)
            {
            case '/':
                return null;
            }
            version (Windows)
            {
            case '\\':
            case ':':
            case '/':
                return null;
            }
            default:
                continue;
            }
        }
        return null;
    }

    unittest
    {
        assert(ext("/foo/bar/dmd.conf"[]) == "conf");
        assert(ext("object.o"[]) == "o");
        assert(ext("/foo/bar/dmd"[]) == null);
        assert(ext(".objdir.o/object"[]) == null);
        assert(ext([]) == null);
    }

    extern (C++) const(char)* ext() const pure @nogc
    {
        return ext(str).ptr;
    }

    /********************************
     * Return file name without extension.
     *
     * TODO:
     * Once slice are used everywhere and `\0` is not assumed,
     * this can be turned into a simple slicing.
     *
     * Params:
     *  str = file name
     *
     * Returns:
     *  mem.xmalloc'd filename with extension removed.
     */
    extern (C++) static const(char)* removeExt(const(char)* str)
    {
        return removeExt(str.toDString).ptr;
    }

    /// Ditto
    extern (D) static const(char)[] removeExt(const char[] str)
    {
        auto e = ext(str);
        if (e.length)
        {
            const len = (str.length - e.length) - 1; // -1 for the dot
            char* n = cast(char*)mem.xmalloc(len + 1);
            memcpy(n, str.ptr, len);
            n[len] = 0;
            return n[0 .. len];
        }
        return mem.xstrdup(str.ptr)[0 .. str.length];
    }

    unittest
    {
        assert(removeExt("/foo/bar/object.d"[]) == "/foo/bar/object");
        assert(removeExt("/foo/bar/frontend.di"[]) == "/foo/bar/frontend");
    }

    /********************************
     * Return filename name excluding path (read-only).
     */
    extern (C++) static const(char)* name(const(char)* str) pure @nogc
    {
        return name(str.toDString).ptr;
    }

    /// Ditto
    extern (D) static const(char)[] name(const char[] str) pure @nogc @safe
    {
        foreach_reverse (idx, char e; str)
        {
            switch (e)
            {
                version (Posix)
                {
                case '/':
                    return str[idx + 1 .. $];
                }
                version (Windows)
                {
                case '/':
                case '\\':
                    return str[idx + 1 .. $];
                case ':':
                    /* The ':' is a drive letter only if it is the second
                     * character or the last character,
                     * otherwise it is an ADS (Alternate Data Stream) separator.
                     * Consider ADS separators as part of the file name.
                     */
                    if (idx == 1 || idx == str.length - 1)
                        return str[idx + 1 .. $];
                    break;
                }
            default:
                break;
            }
        }
        return str;
    }

    extern (C++) const(char)* name() const pure @nogc
    {
        return name(str).ptr;
    }

    unittest
    {
        assert(name("/foo/bar/object.d"[]) == "object.d");
        assert(name("/foo/bar/frontend.di"[]) == "frontend.di");
    }

    /**************************************
     * Return path portion of str.
     * returned string is newly allocated
     * Path does not include trailing path separator.
     */
    extern (C++) static const(char)* path(const(char)* str)
    {
        return path(str.toDString).ptr;
    }

    /// Ditto
    extern (D) static const(char)[] path(const char[] str)
    {
        const n = name(str);
        bool hasTrailingSlash;
        if (n.length < str.length)
        {
            if (isDirSeparator(str[$ - n.length - 1]))
                hasTrailingSlash = true;
        }
        const pathlen = str.length - n.length - (hasTrailingSlash ? 1 : 0);
        char* path = cast(char*)mem.xmalloc(pathlen + 1);
        memcpy(path, str.ptr, pathlen);
        path[pathlen] = 0;
        return path[0 .. pathlen];
    }

    unittest
    {
        assert(path("/foo/bar"[]) == "/foo");
        assert(path("foo"[]) == "");
    }

    /**************************************
     * Replace filename portion of path.
     */
    extern (D) static const(char)[] replaceName(const char[] path, const char[] name)
    {
        if (absolute(name))
            return name;
        auto n = FileName.name(path);
        if (n == path)
            return name;
        return combine(path[0 .. $ - n.length], name);
    }

    /**
       Combine a `path` and a file `name`

       Params:
         path = Path to append to
         name = Name to append to path

       Returns:
         The `\0` terminated string which is the combination of `path` and `name`
         and a valid path.
    */
    extern (C++) static const(char)* combine(const(char)* path, const(char)* name)
    {
        if (!path)
            return name;
        return combine(path.toDString, name.toDString).ptr;
    }

    /// Ditto
    extern(D) static const(char)[] combine(const char[] path, const char[] name)
    {
        return !path.length ? name : buildPath(path, name);
    }

    unittest
    {
        version (Windows)
            assert(combine("foo"[], "bar"[]) == "foo\\bar");
        else
            assert(combine("foo"[], "bar"[]) == "foo/bar");
        assert(combine("foo/"[], "bar"[]) == "foo/bar");
    }

    static const(char)[] buildPath(const char[][] fragments...)
    {
        size_t size;
        foreach (f; fragments)
            size += f.length ? f.length + 1 : 0;
        if (size == 0)
            size = 1;

        char* p = cast(char*) mem.xmalloc_noscan(size);
        size_t length;
        foreach (f; fragments)
        {
            if (!f.length)
                continue;

            p[length .. length + f.length] = f;
            length += f.length;

            const last = p[length - 1];
            version (Posix)
            {
                if (!isDirSeparator(last))
                    p[length++] = '/';
            }
            else version (Windows)
            {
                if (!isDirSeparator(last) && last != ':')
                    p[length++] = '\\';
            }
            else
                assert(0);
        }

        // overwrite last slash with null terminator
        p[length ? --length : 0] = 0;

        return p[0 .. length];
    }

    unittest
    {
        assert(buildPath() == "");
        assert(buildPath("foo") == "foo");
        assert(buildPath("foo", null) == "foo");
        assert(buildPath(null, "foo") == "foo");
        version (Windows)
            assert(buildPath("C:", r"a\", "bb/", "ccc", "d") == r"C:a\bb/ccc\d");
        else
            assert(buildPath("a/", "bb", "ccc") == "a/bb/ccc");
    }

    // Split a path into an Array of paths
    extern (C++) static Strings* splitPath(const(char)* path)
    {
        auto array = new Strings();
        int sink(const(char)* p) nothrow
        {
            array.push(p);
            return 0;
        }
        splitPath(&sink, path);
        return array;
    }

    /****
     * Split path (such as that returned by `getenv("PATH")`) into pieces, each piece is mem.xmalloc'd
     * Handle double quotes and ~.
     * Pass the pieces to sink()
     * Params:
     *  sink = send the path pieces here, end when sink() returns !=0
     *  path = the path to split up.
     */
    static void splitPath(int delegate(const(char)*) nothrow sink, const(char)* path)
    {
        if (!path)
            return;

        auto p = path;
        OutBuffer buf;
        char c;
        do
        {
            const(char)* home;
            bool instring = false;
            while (isspace(*p)) // skip leading whitespace
                ++p;
            buf.reserve(8); // guess size of piece
            for (;; ++p)
            {
                c = *p;
                switch (c)
                {
                    case '"':
                        instring ^= false; // toggle inside/outside of string
                        continue;

                    version (OSX)
                    {
                    case ',':
                    }
                    version (Windows)
                    {
                    case ';':
                    }
                    version (Posix)
                    {
                    case ':':
                    }
                        p++;    // ; cannot appear as part of a
                        break;  // path, quotes won't protect it

                    case 0x1A:  // ^Z means end of file
                    case 0:
                        break;

                    case '\r':
                        continue;  // ignore carriage returns

                    version (Posix)
                    {
                    case '~':
                        if (!home)
                            home = getenv("HOME");
                        // Expand ~ only if it is prefixing the rest of the path.
                        if (!buf.length && p[1] == '/' && home)
                            buf.writestring(home);
                        else
                            buf.writeByte('~');
                        continue;
                    }

                    version (none)
                    {
                    case ' ':
                    case '\t':         // tabs in filenames?
                        if (!instring) // if not in string
                            break;     // treat as end of path
                    }
                    default:
                        buf.writeByte(c);
                        continue;
                }
                break;
            }
            if (buf.length) // if path is not empty
            {
                if (sink(buf.extractChars()))
                    break;
            }
        } while (c);
    }

    /**
     * Add the extension `ext` to `name`, regardless of the content of `name`
     *
     * Params:
     *   name = Path to append the extension to
     *   ext  = Extension to add (should not include '.')
     *
     * Returns:
     *   A newly allocated string (free with `FileName.free`)
     */
    extern(D) static char[] addExt(const char[] name, const char[] ext) pure
    {
        const len = name.length + ext.length + 2;
        auto s = cast(char*)mem.xmalloc(len);
        s[0 .. name.length] = name[];
        s[name.length] = '.';
        s[name.length + 1 .. len - 1] = ext[];
        s[len - 1] = '\0';
        return s[0 .. len - 1];
    }


    /***************************
     * Free returned value with FileName::free()
     */
    extern (C++) static const(char)* defaultExt(const(char)* name, const(char)* ext)
    {
        return defaultExt(name.toDString, ext.toDString).ptr;
    }

    /// Ditto
    extern (D) static const(char)[] defaultExt(const char[] name, const char[] ext)
    {
        auto e = FileName.ext(name);
        if (e.length) // it already has an extension
            return name.xarraydup;
        return addExt(name, ext);
    }

    unittest
    {
        assert(defaultExt("/foo/object.d"[], "d") == "/foo/object.d");
        assert(defaultExt("/foo/object"[], "d") == "/foo/object.d");
        assert(defaultExt("/foo/bar.d"[], "o") == "/foo/bar.d");
    }

    /***************************
     * Free returned value with FileName::free()
     */
    extern (C++) static const(char)* forceExt(const(char)* name, const(char)* ext)
    {
        return forceExt(name.toDString, ext.toDString).ptr;
    }

    /// Ditto
    extern (D) static const(char)[] forceExt(const char[] name, const char[] ext)
    {
        if (auto e = FileName.ext(name))
            return addExt(name[0 .. $ - e.length - 1], ext);
        return defaultExt(name, ext); // doesn't have one
    }

    unittest
    {
        assert(forceExt("/foo/object.d"[], "d") == "/foo/object.d");
        assert(forceExt("/foo/object"[], "d") == "/foo/object.d");
        assert(forceExt("/foo/bar.d"[], "o") == "/foo/bar.o");
    }

    /// Returns:
    ///   `true` if `name`'s extension is `ext`
    extern (C++) static bool equalsExt(const(char)* name, const(char)* ext) pure @nogc
    {
        return equalsExt(name.toDString, ext.toDString);
    }

    /// Ditto
    extern (D) static bool equalsExt(const char[] name, const char[] ext) pure @nogc
    {
        auto e = FileName.ext(name);
        if (!e.length && !ext.length)
            return true;
        if (!e.length || !ext.length)
            return false;
        return FileName.equals(e, ext);
    }

    unittest
    {
        assert(!equalsExt("foo.bar"[], "d"));
        assert(equalsExt("foo.bar"[], "bar"));
        assert(equalsExt("object.d"[], "d"));
        assert(!equalsExt("object"[], "d"));
    }

    /******************************
     * Return !=0 if extensions match.
     */
    extern (C++) bool equalsExt(const(char)* ext) const pure @nogc
    {
        return equalsExt(str, ext.toDString());
    }

    /*************************************
     * Search paths for file.
     * Params:
     *  path = array of path strings
     *  name = file to look for
     *  cwd = true means search current directory before searching path
     * Returns:
     *  if found, filename combined with path, otherwise null
     */
    extern (C++) static const(char)* searchPath(const ref Strings path, const(char)* name, bool cwd)
    {
        return searchPath(path[], name.toDString, cwd).ptr;
    }

    extern (D) static const(char)[] searchPath(const char*[] path, const char[] name, bool cwd)
    {
        if (absolute(name))
        {
            return exists(name) ? name : null;
        }
        if (cwd)
        {
            if (exists(name))
                return name;
        }
        foreach (p; path)
        {
            auto n = combine(p.toDString, name);
            if (exists(n))
                return n;
            //combine might return name
            if (n.ptr != name.ptr)
            {
                mem.xfree(cast(void*)n.ptr);
            }
        }
        return null;
    }

    extern (D) static const(char)[] searchPath(const char* path, const char[] name, bool cwd)
    {
        if (absolute(name))
        {
            return exists(name) ? name : null;
        }
        if (cwd)
        {
            if (exists(name))
                return name;
        }
        if (path && *path)
        {
            const(char)[] result;

            int sink(const(char)* p) nothrow
            {
                auto n = combine(p.toDString, name);
                mem.xfree(cast(void*)p);
                if (exists(n))
                {
                    result = n;
                    return 1;   // done with splitPath() call
                }
                return 0;
            }

            splitPath(&sink, path);
            return result;
        }
        return null;
    }

    /************************************
     * Determine if path contains reserved character.
     * Params:
     *  name = path
     * Returns:
     *  index of the first reserved character in path if found, size_t.max otherwise
     */
    extern (D) static size_t findReservedChar(const char[] name) pure @nogc @safe
    {
        version (Windows)
        {
            // According to https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions
            // the following characters are not allowed in path: < > : " | ? *
            foreach (idx; 0 .. name.length)
            {
                char c = name[idx];
                if (c == '<' || c == '>' || c == ':' || c == '"' || c == '|' || c == '?' || c == '*')
                {
                    return idx;
                }
            }
            return size_t.max;
        }
        else
        {
            return size_t.max;
        }
    }
    unittest
    {
        assert(findReservedChar(r"") == size_t.max);
        assert(findReservedChar(r" abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.-_=+()") == size_t.max);

        version (Windows)
        {
            assert(findReservedChar(` < `) == 1);
            assert(findReservedChar(` >`) == 1);
            assert(findReservedChar(`: `) == 0);
            assert(findReservedChar(`"`) == 0);
            assert(findReservedChar(`|`) == 0);
            assert(findReservedChar(`?`) == 0);
            assert(findReservedChar(`*`) == 0);
        }
        else
        {
            assert(findReservedChar(`<>:"|?*`) == size_t.max);
        }
    }

    /************************************
     * Determine if path has a reference to parent directory.
     * Params:
     *  name = path
     * Returns:
     *  true if path contains '..' reference to parent directory
     */
    extern (D) static bool refersToParentDir(const char[] name) pure @nogc @safe
    {
        size_t s = 0;
        foreach (i; 0 .. name.length)
        {
            if (isDirSeparator(name[i]))
            {
                if (name[s..i] == "..")
                    return true;
                s = i + 1;
            }
        }
        if (name[s..$] == "..")
            return true;

        return false;
    }
    unittest
    {
        assert(!refersToParentDir(r""));
        assert(!refersToParentDir(r"foo"));
        assert(!refersToParentDir(r"foo.."));
        assert(!refersToParentDir(r"foo..boo"));
        assert(!refersToParentDir(r"foo/..boo"));
        assert(!refersToParentDir(r"foo../boo"));
        assert(refersToParentDir(r".."));
        assert(refersToParentDir(r"../"));
        assert(refersToParentDir(r"foo/.."));
        assert(refersToParentDir(r"foo/../"));
        assert(refersToParentDir(r"foo/../../boo"));

        version (Windows)
        {
            // Backslash as directory separator
            assert(!refersToParentDir(r"foo\..boo"));
            assert(!refersToParentDir(r"foo..\boo"));
            assert(refersToParentDir(r"..\"));
            assert(refersToParentDir(r"foo\.."));
            assert(refersToParentDir(r"foo\..\"));
            assert(refersToParentDir(r"foo\..\..\boo"));
        }
    }


    /**
       Check if the file the `path` points to exists

       Returns:
         0 if it does not exists
         1 if it exists and is not a directory
         2 if it exists and is a directory
     */
    extern (C++) static int exists(const(char)* name)
    {
        return exists(name.toDString);
    }

    /// Ditto
    extern (D) static int exists(const char[] name)
    {
        if (!name.length)
            return 0;
        version (Posix)
        {
            stat_t st;
            if (name.toCStringThen!((v) => stat(v.ptr, &st)) < 0)
                return 0;
            if (S_ISDIR(st.st_mode))
                return 2;
            return 1;
        }
        else version (Windows)
        {
            return name.extendedPathThen!((wname)
            {
                const dw = GetFileAttributesW(&wname[0]);
                if (dw == -1)
                    return 0;
                else if (dw & FILE_ATTRIBUTE_DIRECTORY)
                    return 2;
                else
                    return 1;
            });
        }
        else
        {
            assert(0);
        }
    }

    /**
       Ensure that the provided path exists

       Accepts a path to either a file or a directory.
       In the former case, the basepath (path to the containing directory)
       will be checked for existence, and created if it does not exists.
       In the later case, the directory pointed to will be checked for existence
       and created if needed.

       Params:
         path = a path to a file or a directory

       Returns:
         `true` if the directory exists or was successfully created
     */
    extern (D) static bool ensurePathExists(const char[] path)
    {
        //printf("FileName::ensurePathExists(%s)\n", path ? path : "");
        if (!path.length)
            return true;
        if (exists(path))
            return true;

        // We were provided with a file name
        // We need to call ourselves recursively to ensure parent dir exist
        const char[] p = FileName.path(path);
        if (p.length)
        {
            version (Windows)
            {
                // Note: Windows filename comparison should be case-insensitive,
                // however p is a subslice of path so we don't need it
                if (path.length == p.length ||
                    (path.length > 2 && path[1] == ':' && path[2 .. $] == p))
                {
                    mem.xfree(cast(void*)p.ptr);
                    return true;
                }
            }
            const r = ensurePathExists(p);
            mem.xfree(cast(void*)p);

            if (!r)
                return r;
        }

        version (Windows)
            const r = _mkdir(path);
        version (Posix)
        {
            errno = 0;
            const r = path.toCStringThen!((pathCS) => mkdir(pathCS.ptr, (7 << 6) | (7 << 3) | 7));
        }

        if (r == 0)
            return true;

        // Don't error out if another instance of dmd just created
        // this directory
        version (Windows)
        {
            import core.sys.windows.winerror : ERROR_ALREADY_EXISTS;
            if (GetLastError() == ERROR_ALREADY_EXISTS)
                return true;
        }
        version (Posix)
        {
            if (errno == EEXIST)
                return true;
        }

        return false;
    }

    ///ditto
    extern (C++) static bool ensurePathExists(const(char)* path)
    {
        return ensurePathExists(path.toDString);
    }

    /******************************************
     * Return canonical version of name.
     * This code is high risk.
     */
    extern (C++) static const(char)* canonicalName(const(char)* name)
    {
        return canonicalName(name.toDString).ptr;
    }

    /// Ditto
    extern (D) static const(char)[] canonicalName(const char[] name)
    {
        version (Posix)
        {
            import core.stdc.limits;      // PATH_MAX
            import core.sys.posix.unistd; // _PC_PATH_MAX

            // Older versions of druntime don't have PATH_MAX defined.
            // i.e: dmd __VERSION__ < 2085, gdc __VERSION__ < 2076.
            static if (!__traits(compiles, PATH_MAX))
            {
                version (DragonFlyBSD)
                    enum PATH_MAX = 1024;
                else version (FreeBSD)
                    enum PATH_MAX = 1024;
                else version (linux)
                    enum PATH_MAX = 4096;
                else version (NetBSD)
                    enum PATH_MAX = 1024;
                else version (OpenBSD)
                    enum PATH_MAX = 1024;
                else version (OSX)
                    enum PATH_MAX = 1024;
                else version (Solaris)
                    enum PATH_MAX = 1024;
            }

            // Have realpath(), passing a NULL destination pointer may return an
            // internally malloc'd buffer, however it is implementation defined
            // as to what happens, so cannot rely on it.
            static if (__traits(compiles, PATH_MAX))
            {
                // Have compile time limit on filesystem path, use it with realpath.
                char[PATH_MAX] buf = void;
                auto path = name.toCStringThen!((n) => realpath(n.ptr, buf.ptr));
                if (path !is null)
                    return xarraydup(path.toDString);
            }
            else static if (__traits(compiles, canonicalize_file_name))
            {
                // Have canonicalize_file_name, which malloc's memory.
                // We need a dmd.root.rmem allocation though.
                auto path = name.toCStringThen!((n) => canonicalize_file_name(n.ptr));
                scope(exit) .free(path);
                if (path !is null)
                    return xarraydup(path.toDString);
            }
            else static if (__traits(compiles, _PC_PATH_MAX))
            {
                // Panic! Query the OS for the buffer limit.
                auto path_max = pathconf("/", _PC_PATH_MAX);
                if (path_max > 0)
                {
                    char *buf = cast(char*)mem.xmalloc(path_max);
                    scope(exit) mem.xfree(buf);
                    auto path = name.toCStringThen!((n) => realpath(n.ptr, buf));
                    if (path !is null)
                        return xarraydup(path.toDString);
                }
            }
            // Give up trying to support this platform, just duplicate the filename
            // unless there is nothing to copy from.
            if (!name.length)
                return null;
            return xarraydup(name);
        }
        else version (Windows)
        {
            // Convert to wstring first since otherwise the Win32 APIs have a character limit
            return name.toWStringzThen!((wname)
            {
                /* Apparently, there is no good way to do this on Windows.
                 * GetFullPathName isn't it, but use it anyway.
                 */
                // First find out how long the buffer has to be, incl. terminating null.
                const capacity = GetFullPathNameW(&wname[0], 0, null, null);
                if (!capacity) return null;
                auto buffer = cast(wchar*) mem.xmalloc_noscan(capacity * wchar.sizeof);
                scope(exit) mem.xfree(buffer);

                // Actually get the full path name. If the buffer is large enough,
                // the returned length does NOT include the terminating null...
                const length = GetFullPathNameW(&wname[0], capacity, buffer, null /*filePart*/);
                assert(length == capacity - 1);

                return toNarrowStringz(buffer[0 .. length]);
            });
        }
        else
        {
            assert(0);
        }
    }

    unittest
    {
        string filename = "foo.bar";
        const path = canonicalName(filename);
        scope(exit) free(path.ptr);
        assert(path.length >= filename.length);
        assert(path[$ - filename.length .. $] == filename);
    }

    /********************************
     * Free memory allocated by FileName routines
     */
    extern (C++) static void free(const(char)* str) pure
    {
        if (str)
        {
            assert(str[0] != cast(char)0xAB);
            memset(cast(void*)str, 0xAB, strlen(str) + 1); // stomp
        }
        mem.xfree(cast(void*)str);
    }

    extern (C++) const(char)* toChars() const pure nothrow @nogc @trusted
    {
        // Since we can return an empty slice (but '\0' terminated),
        // we don't do bounds check (as `&str[0]` does)
        return str.ptr;
    }

    const(char)[] toString() const pure nothrow @nogc @trusted
    {
        return str;
    }

    bool opCast(T)() const pure nothrow @nogc @safe
    if (is(T == bool))
    {
        return str.ptr !is null;
    }
}

version(Windows)
{
    /****************************************************************
     * The code before used the POSIX function `mkdir` on Windows. That
     * function is now deprecated and fails with long paths, so instead
     * we use the newer `CreateDirectoryW`.
     *
     * `CreateDirectoryW` is the unicode version of the generic macro
     * `CreateDirectory`.  `CreateDirectoryA` has a file path
     *  limitation of 248 characters, `mkdir` fails with less and might
     *  fail due to the number of consecutive `..`s in the
     *  path. `CreateDirectoryW` also normally has a 248 character
     * limit, unless the path is absolute and starts with `\\?\`. Note
     * that this is different from starting with the almost identical
     * `\\?`.
     *
     * Params:
     *  path = The path to create.
     *
     * Returns:
     *  0 on success, 1 on failure.
     *
     * References:
     *  https://msdn.microsoft.com/en-us/library/windows/desktop/aa363855(v=vs.85).aspx
     */
    private int _mkdir(const char[] path) nothrow
    {
        const createRet = path.extendedPathThen!(
            p => CreateDirectoryW(&p[0], null /*securityAttributes*/));
        // different conventions for CreateDirectory and mkdir
        return createRet == 0 ? 1 : 0;
    }

    /**********************************
     * Converts a UTF-16 string to a (null-terminated) narrow string.
     * Returns:
     *  If `buffer` is specified and the result fits, a slice of that buffer,
     *  otherwise a new buffer which can be released via `mem.xfree()`.
     *  Nulls are propagated, i.e., if `wide` is null, the returned slice is
     *  null too.
     */
    char[] toNarrowStringz(const(wchar)[] wide, char[] buffer = null) nothrow
    {
        import dmd.common.file : CodePage;

        if (wide is null)
            return null;

        const requiredLength = WideCharToMultiByte(CodePage, 0, wide.ptr, cast(int) wide.length, buffer.ptr, cast(int) buffer.length, null, null);
        if (requiredLength < buffer.length)
        {
            buffer[requiredLength] = 0;
            return buffer[0 .. requiredLength];
        }

        char* newBuffer = cast(char*) mem.xmalloc_noscan(requiredLength + 1);
        const length = WideCharToMultiByte(CodePage, 0, wide.ptr, cast(int) wide.length, newBuffer, requiredLength, null, null);
        assert(length == requiredLength);
        newBuffer[length] = 0;
        return newBuffer[0 .. length];
    }

    /**********************************
     * Converts a slice of UTF-8 characters to an array of wchar that's null
     * terminated so it can be passed to Win32 APIs then calls the supplied
     * function on it.
     *
     * Params:
     *  str = The string to convert.
     *
     * Returns:
     *  The result of calling F on the UTF16 version of str.
     */
    private auto toWStringzThen(alias F)(const char[] str) nothrow
    {
        import dmd.common.smallbuffer : SmallBuffer, toWStringz;

        if (!str.length) return F(""w.ptr);

        wchar[1024] support = void;
        auto buf = SmallBuffer!wchar(support.length, support);
        wchar[] wide = toWStringz(str, buf);
        scope(exit) wide.ptr != buf.ptr && mem.xfree(wide.ptr);

        return F(wide);
    }
}
