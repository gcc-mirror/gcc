// RUNNABLE_PHOBOS_TEST
import std.stdio;

/***********************************/

void test1()
{
    int i;
    __gshared int j;

    version (D_InlineAsm_X86)
    {
        asm
        {
            naked       ;
            mov EAX, i  ;
        }
      version(D_PIC)
      {}
      else
      {
        asm
        {
            mov EAX, j  ;
        }
      }
    }
}

/***********************************/

int main()
{
    for (int i = 0; ; i++)
    {
        if (i == 10)
            break;
    }

    string[] a = new string[3];
    a[0] = "hello";
    a[1] = "world";
    a[2] = "foo";

    foreach (string s; a)
        writefln(s);

    switch (1)
    {
        default:
            break;
    }

    switch ("foo"w)
    {
        case "foo":
            break;
        default: assert(0);
    }

    switch (1)
    {
        case 1:
            try
            {
                goto default;
            }
            catch (Throwable o)
            {
            }
            break;

        default:
            break;
    }

    switch (1)
    {
        case 1:
            try
            {
                goto case 2;
            }
            catch (Throwable o)
            {
            }
            break;

        case 2:
            break;

        default: assert(0);
    }

    writefln("Success\n");
    return 0;
}
