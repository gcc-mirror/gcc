/* PR tree-optimization/89720 - Spurious -Warray-bounds warning on a range
   with max < min
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void f (char*, int);

#if __SIZEOF_POINTER__ == 8

void g (__INT64_TYPE__ i)
{
  char a[65536] = "";
  char *p = a + (i & (__INT64_TYPE__)0xffffffff3fffffffLL);
  f (p, *(p - 6));            /* { dg-bogus "\\\[-Warray-bounds" } */
}

#elif __SIZEOF_POINTER__ == 4

void h (__INT32_TYPE__ i)
{
  char a[65536] = "";
  char *p = a + (i & (__INT32_TYPE__)0x8fffffffLL);
  f (p, *(p - 6));            /* { dg-bogus "\\\[-Warray-bounds" } */
}

#endif
