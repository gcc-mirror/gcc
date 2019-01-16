// PERMUTE_ARGS:
// EXECUTE_ARGS: runnable/wc.d

import std.file;

extern(C) int printf(const char*, ...);

int main (string[] args)
{
    int w_total;
    int l_total;
    int c_total;

    printf ("   lines   words   bytes file\n");
    foreach (arg; args[1 .. args.length])
    {
        string input;
        int w_cnt, l_cnt, c_cnt;
        int inword;

        input = cast(string)std.file.read(arg);

        foreach (char c; input)
        {
            if (c == '\n')
                ++l_cnt;
            if (c != ' ')
            {
                if (!inword)
                {
                    inword = 1;
                    ++w_cnt;
                }
            }
            else
                inword = 0;
            ++c_cnt;
        }
        printf ("%8lu%8lu%8lu %.*s\n", l_cnt, w_cnt, c_cnt, arg.length, arg.ptr);
        l_total += l_cnt;
        w_total += w_cnt;
        c_total += c_cnt;
    }
    if (args.length > 2)
    {
        printf ("--------------------------------------\n%8lu%8lu%8lu total",
            l_total, w_total, c_total);
    }
    return 0;
}
