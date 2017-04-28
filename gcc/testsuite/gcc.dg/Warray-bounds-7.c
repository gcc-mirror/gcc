/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

char *p;

int main()
{
  p = "";
  if (p[0] == 0
      || (p[0] == '_' && p[1] == 0))  /* { dg-bogus "array bounds" } */
    return 0;
  return 1;
}
