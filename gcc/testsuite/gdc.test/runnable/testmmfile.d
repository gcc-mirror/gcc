// PERMUTE_ARGS:
// REQUIRED_ARGS:

import std.file;
import std.mmfile;

int main()
{
    static string name = "test.tmp";
    static string s = "abcd";

    write(name, s);

    {   scope MmFile mmf = new MmFile(name);
        string p;

        assert(mmf[0] == 'a');
        p = cast(string)mmf[];
        //printf("p.length = %d\n", p.length);
        assert(p[1] == 'b');
        p = cast(string)mmf[0 .. 4];
        assert(p[2] == 'c');
    }

    {   scope MmFile mmf = new MmFile(name, MmFile.Mode.read, 0, null);
        string p;

        assert(mmf[0] == 'a');
        p = cast(string)mmf[];
        //printf("p.length = %d\n", p.length);
        assert(mmf.length == 4);
        assert(p[1] == 'b');
        p = cast(string)mmf[0 .. 4];
        assert(p[2] == 'c');
    }

    remove(name);

    {   scope MmFile mmf = new MmFile(name, MmFile.Mode.readWriteNew, 4, null);
        char[] p;

        p = cast(char[])mmf[];
        p[] = "1234";
        mmf[3] = '5';
        assert(mmf[2] == '3');
        assert(mmf[3] == '5');
    }

    {   string p = cast(string)read(name);

        assert(p[] == "1235");
    }

    {   scope MmFile mmf = new MmFile(name, MmFile.Mode.readWriteNew, 4, null);
        char[] p;

        p = cast(char[])mmf[];
        p[] = "5678";
        mmf[3] = '5';
        assert(mmf[2] == '7');
        assert(mmf[3] == '5');
        assert(cast(string)mmf[] == "5675");
    }

    {   string p = cast(string)read(name);

        assert(p[] == "5675");
    }

    {   scope MmFile mmf = new MmFile(name, MmFile.Mode.readWrite, 4, null);
        char[] p;

        p = cast(char[])mmf[];
        assert(cast(char[])mmf[] == "5675");
        p[] = "9102";
        mmf[2] = '5';
        assert(cast(string)mmf[] == "9152");
    }

    {   string p = cast(string)read(name);

        assert(p[] == "9152");
    }

    remove(name);

    {   scope MmFile mmf = new MmFile(name, MmFile.Mode.readWrite, 4, null);
        char[] p;

        p = cast(char[])mmf[];
        p[] = "abcd";
        mmf[2] = '5';
        assert(cast(string)mmf[] == "ab5d");
    }

    {   string p = cast(string)read(name);

        assert(p[] == "ab5d");
    }

    {   scope MmFile mmf = new MmFile(name, MmFile.Mode.readCopyOnWrite, 4, null);
        char[] p;

        p = cast(char[])mmf[];
        assert(cast(string)mmf[] == "ab5d");
        p[] = "9102";
        mmf[2] = '5';
        assert(cast(string)mmf[] == "9152");
    }

    {   string p = cast(string)read(name);

        assert(p[] == "ab5d");
    }

    remove(name);

    return 0;
}
