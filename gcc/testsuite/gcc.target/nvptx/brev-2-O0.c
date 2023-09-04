/* { dg-do run } */
/* { dg-options "-O0" } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {**} {} } } */

inline __attribute__((always_inline))
unsigned int bitreverse32(unsigned int x)
{
  return __builtin_nvptx_brev(x);
}

int main(void)
{
  if (bitreverse32(0x00000000) != 0x00000000)
    __builtin_abort();
  if (bitreverse32(0xffffffff) != 0xffffffff)
    __builtin_abort();

  if (bitreverse32(0x00000001) != 0x80000000)
    __builtin_abort();
  if (bitreverse32(0x00000002) != 0x40000000)
    __builtin_abort();
  if (bitreverse32(0x00000004) != 0x20000000)
    __builtin_abort();
  if (bitreverse32(0x00000008) != 0x10000000)
    __builtin_abort();
  if (bitreverse32(0x00000010) != 0x08000000)
    __builtin_abort();
  if (bitreverse32(0x00000020) != 0x04000000)
    __builtin_abort();
  if (bitreverse32(0x00000040) != 0x02000000)
    __builtin_abort();
  if (bitreverse32(0x00000080) != 0x01000000)
    __builtin_abort();
  if (bitreverse32(0x00000100) != 0x00800000)
    __builtin_abort();
  if (bitreverse32(0x00000200) != 0x00400000)
    __builtin_abort();
  if (bitreverse32(0x00000400) != 0x00200000)
    __builtin_abort();
  if (bitreverse32(0x00000800) != 0x00100000)
    __builtin_abort();
  if (bitreverse32(0x00001000) != 0x00080000)
    __builtin_abort();
  if (bitreverse32(0x00002000) != 0x00040000)
    __builtin_abort();
  if (bitreverse32(0x00004000) != 0x00020000)
    __builtin_abort();
  if (bitreverse32(0x00008000) != 0x00010000)
    __builtin_abort();
  if (bitreverse32(0x00010000) != 0x00008000)
    __builtin_abort();
  if (bitreverse32(0x00020000) != 0x00004000)
    __builtin_abort();
  if (bitreverse32(0x00040000) != 0x00002000)
    __builtin_abort();
  if (bitreverse32(0x00080000) != 0x00001000)
    __builtin_abort();
  if (bitreverse32(0x00100000) != 0x00000800)
    __builtin_abort();
  if (bitreverse32(0x00200000) != 0x00000400)
    __builtin_abort();
  if (bitreverse32(0x00400000) != 0x00000200)
    __builtin_abort();
  if (bitreverse32(0x00800000) != 0x00000100)
    __builtin_abort();
  if (bitreverse32(0x01000000) != 0x00000080)
    __builtin_abort();
  if (bitreverse32(0x02000000) != 0x00000040)
    __builtin_abort();
  if (bitreverse32(0x04000000) != 0x00000020)
    __builtin_abort();
  if (bitreverse32(0x08000000) != 0x00000010)
    __builtin_abort();
  if (bitreverse32(0x10000000) != 0x00000008)
    __builtin_abort();
  if (bitreverse32(0x20000000) != 0x00000004)
    __builtin_abort();
  if (bitreverse32(0x40000000) != 0x00000002)
    __builtin_abort();
  if (bitreverse32(0x80000000) != 0x00000001)
    __builtin_abort();

  if (bitreverse32(0x01234567) != 0xe6a2c480)
    __builtin_abort();
  if (bitreverse32(0xe6a2c480) != 0x01234567)
    __builtin_abort();
  if (bitreverse32(0xdeadbeef) != 0xf77db57b)
    __builtin_abort();
  if (bitreverse32(0xf77db57b) != 0xdeadbeef)
    __builtin_abort();
  if (bitreverse32(0xcafebabe) != 0x7d5d7f53)
    __builtin_abort();
  if (bitreverse32(0x7d5d7f53) != 0xcafebabe)
    __builtin_abort();

  return 0;
}
/*
** main:
**	...
**	mov\.u32	(%r[0-9]+), 0;
**	st\.u32	(\[%frame[+0-9]*\]), \1;
**	ld\.u32	(%r[0-9]+), \2;
**	brev\.b32	(%r[0-9]+), \3;
**	setp\.[^.]+\.u32	%r[0-9]+, \4, 0;
**	...
**	mov\.u32	(%r[0-9]+), -1;
**	st\.u32	(\[%frame[+0-9]*\]), \5;
**	ld\.u32	(%r[0-9]+), \6;
**	brev\.b32	(%r[0-9]+), \7;
**	setp\.[^.]+\.u32	%r[0-9]+, \8, -1;
**	...
**	mov\.u32	(%r[0-9]+), 1;
**	st\.u32	(\[%frame[+0-9]*\]), \9;
**	ld\.u32	(%r[0-9]+), \10;
**	brev\.b32	(%r[0-9]+), \11;
**	setp\.[^.]+\.u32	%r[0-9]+, \12, -2147483648;
**	...
**	mov\.u32	(%r[0-9]+), 2;
**	st\.u32	(\[%frame[+0-9]*\]), \13;
**	ld\.u32	(%r[0-9]+), \14;
**	brev\.b32	(%r[0-9]+), \15;
**	setp\.[^.]+\.u32	%r[0-9]+, \16, 1073741824;
**	...
*/

/* { dg-final { scan-assembler-times {\tbrev\.b32\t} 40 } } */
/* { dg-final { scan-assembler {\mabort\M} } } */
