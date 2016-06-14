/* { dg-do run } */

#if __SIZEOF_LONG_DOUBLE__ == 16
#define STR "AAAAAAAAAAAAAAA"
#elif __SIZEOF_LONG_DOUBLE__ == 12
#define STR "AAAAAAAAAAA"
#elif __SIZEOF_LONG_DOUBLE__ == 8
#define STR "AAAAAAA"
#elif __SIZEOF_LONG_DOUBLE__ == 4
#define STR "AAA"
#else
#define STR "A"
#endif

int main()
{
  long double d;
  char s[sizeof d];

  __builtin_memcpy(&d, STR, sizeof d);
  __builtin_memcpy(&s, &d, sizeof s);

  if (__builtin_strncmp (s, STR, sizeof s) != 0)
    __builtin_abort ();

  return 0;
}
