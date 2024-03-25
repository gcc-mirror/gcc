/* { dg-skip-if "small alignment" { pdp11-*-* } } */

void abort (void);

void func(void) __attribute__((aligned(256)));

void func(void) 
{
}

int main()
{
  if (__alignof__(func) != 256)
    abort ();
  return 0;
}
