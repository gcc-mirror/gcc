/* { dg-do compile } */
/* { dg-options "-O0 -Wall" } */

int main()
{
  char foo[3];
  int i;

  for (i = 0; i < 16; i++)
    __builtin_snprintf(foo, sizeof(foo), "%d", i);  /* { dg-bogus "truncated" } */
}
