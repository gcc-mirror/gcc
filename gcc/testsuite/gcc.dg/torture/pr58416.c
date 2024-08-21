/* { dg-do run } */

struct s {
  char s[sizeof(long double)];
};

union u {
  long double d;
  struct s s;
};

int main()
{
  union u x = {0};
#if __SIZEOF_LONG_DOUBLE__ == 16
  x.s = (struct s){"xxxxxxxxxxxxxxxx"};
#elif __SIZEOF_LONG_DOUBLE__ == 12
  x.s = (struct s){"xxxxxxxxxxxx"};
#elif __SIZEOF_LONG_DOUBLE__ == 8
  x.s = (struct s){"xxxxxxxx"};
#elif __SIZEOF_LONG_DOUBLE__ == 4
  x.s = (struct s){"xxxx"};
#endif

  union u y = x;

  for (unsigned char *p = (unsigned char *)&y + sizeof y;
       p-- > (unsigned char *)&y;)
    if (*p != (unsigned char)'x')
      __builtin_abort ();
  return 0;
}
