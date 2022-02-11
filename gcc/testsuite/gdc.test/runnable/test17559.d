// REQUIRED_ARGS: -g
// REQUIRED_ARGS(linux freebsd dragonflybsd): -L-export-dynamic
// PERMUTE_ARGS:
// DISABLED: osx

import core.stdc.stdio;

void main()
{
    fun(1);
    fun(2);
    fun(3);
#line 30
    fun(4);

    foo(1, 10);
    foo(2, 10);
    foo(3, 10);
#line 40
    foo(4, 10);
}

void fun(int n, int defParam = 10)
{
    try
    {
        if (n == 4)
            throw new Exception("fun");
    }
    catch(Exception e)
    {
        string s = e.toString();
        printf("%.*s\n", cast(int)s.length, s.ptr);
        int line = lineInMain(e.toString());
        assert(line >= 30 && line <= 32); // return address might be next statement
    }
}

void foo(int n, int m)
{
    try
    {
        if (n == 4)
            throw new Exception("foo");
    }
    catch(Exception e)
    {
        string s = e.toString();
        printf("%.*s\n", cast(int)s.length, s.ptr);
        int line = lineInMain(e.toString());
        assert(line >= 40 && line <= 41); // return address might be next statement
    }
}

int lineInMain(string msg)
{
    // find line number of _Dmain in stack trace
    // on linux:   file.d:line _Dmain [addr]
    // on windows: addr in _Dmain at file.d(line)
    int line = 0;
    bool mainFound = false;
    for (size_t pos = 0; pos + 6 < msg.length; pos++)
    {
        if (msg[pos] == '\n')
        {
            line = 0;
            mainFound = false;
        }
        else if ((msg[pos] == ':' || msg[pos] == '(') && line == 0)
        {
            for (pos++; pos < msg.length && msg[pos] >= '0' && msg[pos] <= '9'; pos++)
                line = line * 10 + msg[pos] - '0';
            if (line > 0 && mainFound)
                return line;
        }
        else if (msg[pos .. pos + 6] == "_Dmain" || msg[pos .. pos + 6] == "D main")
        {
            mainFound = true;
            if (line > 0 && mainFound)
                return line;
        }
    }
    return 0;
}
