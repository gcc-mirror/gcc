/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -march=cascadelake" } */
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned long long u64;

u8 g;

void
foo (__int128 i, u8 *r)
{
  u16 a = __builtin_sub_overflow_p (0, i * g, 0);
  i ^= g & i;
  u64 s = (i >> 64) + i;
  *r = ((union { u16 a; u8 b[2]; }) a).b[1] + s;
}

int
bar (void)
{
  u8 x;
  foo (5, &x);
  if (x != 5)
    __builtin_abort ();
  return 0;
}
/* { dg-final { scan-assembler-not "andn\[ \\t\]+%rdi, %r11, %rdi" } } */
