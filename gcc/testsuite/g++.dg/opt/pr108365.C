// PR c++/108365
// { dg-do run }

char b = 1;

int
main ()
{
#if __CHAR_BIT__ == 8 && __SIZEOF_SHORT__ == 2 && __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
  while ((short) ((long long) (unsigned long long) (-__INT_MAX__ - 1) / (long long) (b ? -1 : 0)))
    ;
#endif
}
