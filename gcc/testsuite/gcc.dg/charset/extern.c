/* { dg-do compile }
   { dg-require-iconv "IBM-1047" }
   { dg-final { scan-assembler-not "abcdefghijklmnopqrstuvwxyz" } } */

extern char *bar;

extern void foo (void)
{
   char str[]="abcdefghijklmnopqrstuvwxyz";
   bar = str;
}
