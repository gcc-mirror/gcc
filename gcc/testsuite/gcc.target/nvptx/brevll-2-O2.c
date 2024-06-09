/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options -save-temps } */

inline __attribute__((always_inline))
unsigned long long bitreverse64(unsigned long long x)
{
  return __builtin_nvptx_brevll(x);
}

int main(void)
{
  if (bitreverse64(0x0000000000000000ll) != 0x0000000000000000ll)
    __builtin_abort();
  if (bitreverse64(0xffffffffffffffffll) != 0xffffffffffffffffll)
    __builtin_abort();

  if (bitreverse64(0x0000000000000001ll) != 0x8000000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000002ll) != 0x4000000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000004ll) != 0x2000000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000008ll) != 0x1000000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000010ll) != 0x0800000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000020ll) != 0x0400000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000040ll) != 0x0200000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000080ll) != 0x0100000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000100ll) != 0x0080000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000200ll) != 0x0040000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000400ll) != 0x0020000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000000800ll) != 0x0010000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000001000ll) != 0x0008000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000002000ll) != 0x0004000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000004000ll) != 0x0002000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000008000ll) != 0x0001000000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000010000ll) != 0x0000800000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000020000ll) != 0x0000400000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000040000ll) != 0x0000200000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000080000ll) != 0x0000100000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000100000ll) != 0x0000080000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000200000ll) != 0x0000040000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000400000ll) != 0x0000020000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000000800000ll) != 0x0000010000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000001000000ll) != 0x0000008000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000002000000ll) != 0x0000004000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000004000000ll) != 0x0000002000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000008000000ll) != 0x0000001000000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000010000000ll) != 0x0000000800000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000020000000ll) != 0x0000000400000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000040000000ll) != 0x0000000200000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000080000000ll) != 0x0000000100000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000100000000ll) != 0x0000000080000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000200000000ll) != 0x0000000040000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000400000000ll) != 0x0000000020000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000000800000000ll) != 0x0000000010000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000001000000000ll) != 0x0000000008000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000002000000000ll) != 0x0000000004000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000004000000000ll) != 0x0000000002000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000008000000000ll) != 0x0000000001000000ll)
    __builtin_abort();
  if (bitreverse64(0x0000010000000000ll) != 0x0000000000800000ll)
    __builtin_abort();
  if (bitreverse64(0x0000020000000000ll) != 0x0000000000400000ll)
    __builtin_abort();
  if (bitreverse64(0x0000040000000000ll) != 0x0000000000200000ll)
    __builtin_abort();
  if (bitreverse64(0x0000080000000000ll) != 0x0000000000100000ll)
    __builtin_abort();
  if (bitreverse64(0x0000100000000000ll) != 0x0000000000080000ll)
    __builtin_abort();
  if (bitreverse64(0x0000200000000000ll) != 0x0000000000040000ll)
    __builtin_abort();
  if (bitreverse64(0x0000400000000000ll) != 0x0000000000020000ll)
    __builtin_abort();
  if (bitreverse64(0x0000800000000000ll) != 0x0000000000010000ll)
    __builtin_abort();
  if (bitreverse64(0x0001000000000000ll) != 0x0000000000008000ll)
    __builtin_abort();
  if (bitreverse64(0x0002000000000000ll) != 0x0000000000004000ll)
    __builtin_abort();
  if (bitreverse64(0x0004000000000000ll) != 0x0000000000002000ll)
    __builtin_abort();
  if (bitreverse64(0x0008000000000000ll) != 0x0000000000001000ll)
    __builtin_abort();
  if (bitreverse64(0x0010000000000000ll) != 0x0000000000000800ll)
    __builtin_abort();
  if (bitreverse64(0x0020000000000000ll) != 0x0000000000000400ll)
    __builtin_abort();
  if (bitreverse64(0x0040000000000000ll) != 0x0000000000000200ll)
    __builtin_abort();
  if (bitreverse64(0x0080000000000000ll) != 0x0000000000000100ll)
    __builtin_abort();
  if (bitreverse64(0x0100000000000000ll) != 0x0000000000000080ll)
    __builtin_abort();
  if (bitreverse64(0x0200000000000000ll) != 0x0000000000000040ll)
    __builtin_abort();
  if (bitreverse64(0x0400000000000000ll) != 0x0000000000000020ll)
    __builtin_abort();
  if (bitreverse64(0x0800000000000000ll) != 0x0000000000000010ll)
    __builtin_abort();
  if (bitreverse64(0x1000000000000000ll) != 0x0000000000000008ll)
    __builtin_abort();
  if (bitreverse64(0x2000000000000000ll) != 0x0000000000000004ll)
    __builtin_abort();
  if (bitreverse64(0x4000000000000000ll) != 0x0000000000000002ll)
    __builtin_abort();
  if (bitreverse64(0x8000000000000000ll) != 0x0000000000000001ll)
    __builtin_abort();

  if (bitreverse64(0x0123456789abcdefll) != 0xf7b3d591e6a2c480ll)
    __builtin_abort();
  if (bitreverse64(0xf7b3d591e6a2c480ll) != 0x0123456789abcdefll)
    __builtin_abort();
  if (bitreverse64(0xdeadbeefcafebabell) != 0x7d5d7f53f77db57bll)
    __builtin_abort();
  if (bitreverse64(0x7d5d7f53f77db57bll) != 0xdeadbeefcafebabell)
    __builtin_abort();

  return 0;
}

/* { dg-final { scan-assembler-not {\tbrev\.b64\t} } } */
/* { dg-final { scan-assembler-not {\mabort\M} } } */
