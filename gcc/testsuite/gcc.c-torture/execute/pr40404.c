extern void abort (void);

#if (__SIZEOF_INT__ <= 2)
struct S {
  unsigned long ui17 : 17;
} s;
#else
struct S {
  unsigned int ui17 : 17;
} s;
#endif
int main()
{
  s.ui17 = 0x1ffff;
  if (s.ui17 >= 0xfffffffeu)
    abort ();
  return 0;
}

