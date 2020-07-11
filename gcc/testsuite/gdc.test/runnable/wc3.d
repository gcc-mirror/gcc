// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:
// EXECUTE_ARGS: runnable/extra-files/alice30.txt
// EXTRA_FILES: extra-files/alice30.txt

import std.stdio;
import std.file;

int main (string[] args)
{
    int w_total;
    int l_total;
    int c_total;
    int[string] dictionary;

    writefln("   lines   words   bytes file");
    foreach (arg; args[1 .. args.length])
    {
        int w_cnt, l_cnt, c_cnt;
        size_t wstart;
        bool inword;

        auto input = cast(string)std.file.read(arg);

        foreach (j, c; input)
        {
            if (c == '\n')
                ++l_cnt;
            if (c >= '0' && c <= '9')
            {
            }
            else if (c >= 'a' && c <= 'z' ||
                     c >= 'A' && c <= 'Z')
            {
                if (!inword)
                {
                    wstart = j;
                    inword = true;
                    ++w_cnt;
                }
            }
            else if (inword)
            {   auto word = input[wstart .. j];

                dictionary[word]++;
                inword = false;
            }
            ++c_cnt;
        }
        if (inword)
        {   auto w = input[wstart .. input.length];
            dictionary[w]++;
        }
        writefln("%8s%8s%8s %s", l_cnt, w_cnt, c_cnt, arg);
        l_total += l_cnt;
        w_total += w_cnt;
        c_total += c_cnt;
    }

    if (args.length > 2)
    {
        writefln("--------------------------------------\n%8s%8s%8s total",
            l_total, w_total, c_total);
    }

    writefln("--------------------------------------");

    foreach (word1; dictionary.keys)
    {
        writefln("%3s %s", dictionary[word1], word1);
    }
    return 0;
}
