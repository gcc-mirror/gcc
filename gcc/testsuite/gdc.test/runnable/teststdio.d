// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:
// EXTRA_FILES: extra-files/teststdio.txt

import std.stdio;
import core.stdc.stdio;

void main()
{
    auto f = std.stdio.File("runnable/extra-files/teststdio.txt", "r");
    FILE* fp = f.getFP();
    string buf;
    int i;
    do
    {
        buf = f.readln('\n');
        foreach (c; buf)
            printf("%x\n", c);
        printf("\n");
        switch (i)
        {
            case 0:     assert(buf == "asdfasdf\n"); break;
            case 1:     assert(buf == "a\n"); break;
            case 2:     assert(buf == "sdf\n"); break;
            case 3:     assert(buf == "asdf\n"); break;
            case 4:     assert(buf == "\n"); break;
            case 5:     assert(buf == "\n"); break;
            case 6:     assert(buf == null); break;
            default:    assert(0);
        }
        i++;
    } while (!feof(fp));
    //fclose(fp);
}
