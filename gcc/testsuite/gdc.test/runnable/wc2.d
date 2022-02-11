// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:
// EXECUTE_ARGS: runnable/wc2.d

import std.file;

extern(C) int printf(const char*, ...);

int main (string[] args)
{
    int w_total;
    int l_total;
    int c_total;
    int[string] dictionary;

    printf("   lines   words   bytes file\n");
    foreach (string arg; args[1 .. args.length])
    {
        string input;
        int w_cnt, l_cnt, c_cnt;
        int inword;
        int wstart;

        input = cast(string)std.file.read(arg);

        for (int j = 0; j < input.length; j++)
        {   char c;

            c = input[j];
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
                    inword = 1;
                    ++w_cnt;
                }
            }
            else if (inword)
            {   string word = input[wstart .. j];

                dictionary[word]++;
                inword = 0;
            }
            ++c_cnt;
        }
        if (inword)
        {   string w = input[wstart .. input.length];
            dictionary[w]++;
        }
        printf("%8u%8u%8u %.*s\n", l_cnt, w_cnt, c_cnt, cast(int)arg.length, arg.ptr);
        l_total += l_cnt;
        w_total += w_cnt;
        c_total += c_cnt;
    }

    if (args.length > 2)
    {
        printf("--------------------------------------\n%8u%8u%8u total",
            l_total, w_total, c_total);
    }

    printf("--------------------------------------\n");
    foreach (string word1; dictionary.keys)
    {
        printf("%3d %.*s\n", dictionary[word1], cast(int)word1.length, word1.ptr);
    }
    return 0;
}
