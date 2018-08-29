extern void abort (void);
extern void exit (int);

static int f(char *x)
{
   return __builtin_strlen(x);
}

int foo ()
{
   return f((char*)&L"abcdef"[0]);
}


int
main()
{
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  if (foo () != 0)
    abort ();
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if (foo () != 1)
    abort ();
#endif
  exit (0);
}
