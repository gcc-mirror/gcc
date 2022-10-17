/* { dg-do compile } */
/* { dg-options "-fif-conversion2 -Og" } */
/* { dg-options "-fif-conversion2 -Og -march=z9-ec" { target { s390x-*-* } } } */

unsigned char x;
int foo(void)
{
  unsigned long long i = x;
  i = i + 0x80000000;
  unsigned long long t = 0xffffffff;

  if (i > t) {
    unsigned long long ii;
    asm("":"=g"(ii):"0"(i));
    if ((ii <= t))
      __builtin_trap();
    return x;
  }

 return 0;
}
