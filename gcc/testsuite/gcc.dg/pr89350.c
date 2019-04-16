/* PR tree-optimization/89350 - Wrong -Wstringop-overflow warning
   on a variable offset from the end of an array
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char buf[128];
char *src = "HCSparta";

int main(int argc, char **argv)
{
    char *dst = buf + sizeof(buf);

    if (argc)
    {
      dst -= argc;
      __builtin_memcpy(dst, src, argc + 0);   /* { dg-bogus "\\\[-Warray-bounds|-Wstringop-overflow" } */
    }
}
