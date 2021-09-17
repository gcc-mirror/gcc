/* PR middle-end/102200 - ICE on a min of a decl and pointer in a loop
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern char a[], n;

void f (void)
{
  char *p = a;
  size_t end = 1;

  while (n)
    {
      if (p < (char*)end)
        *p = ';';

      if (p > (char*)&end)
        p = (char*)&end;
    }
}
