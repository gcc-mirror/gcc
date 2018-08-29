/* PR tree-optimization/48560 - -Warray-bounds fails to detect the out of
   bound array access
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds" } */

char foo1 (int i)
{
  static char s[] = "foo";
  switch (i)
    {
    case 30:
      return s[30];   /* { dg-warning "array subscript 30 is above array bounds" } */
    }
  return s[i];
}
