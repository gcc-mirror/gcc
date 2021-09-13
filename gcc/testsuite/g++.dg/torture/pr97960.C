// PR tree-optimization/97960
// { dg-do run }

#if __CHAR_BIT__ == 8 && __SIZEOF_INT__ == 4
const int &
foo (const int &d, const int &f)
{
  if (d < f)
    return f;
  return d;
}

short a[575];
unsigned b[25];
unsigned char g;
#endif

int
main ()
{
#if __CHAR_BIT__ == 8 && __SIZEOF_INT__ == 4
  for (int e = 0; e < 23; ++e)
    a[e * 23] = 16137;
  for (signed char h = (unsigned char) (foo (g, 253) + 3); h < 24; h++)
    b[h] = 1064739102;
  for (int e = 0; e < 23; ++e)
    if (a[e * 23] != 16137)
      __builtin_abort ();
#endif
}
