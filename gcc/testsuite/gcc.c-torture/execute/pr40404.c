extern void abort (void);
struct S {
  unsigned int ui17 : 17;
} s;
int main()
{
  s.ui17 = 0x1ffff;
  if (s.ui17 >= 0xfffffffeu)
    abort ();
  return 0;
}

