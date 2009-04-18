/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O" } */

typedef unsigned char u8;
struct __large_struct { unsigned long buf[100]; };
static inline __attribute__((always_inline)) unsigned long
__copy_from_user_inatomic(void *to, const void *from, unsigned long n)
{
  unsigned long ret = 0;
  asm volatile("1:	mov""b"" %2,%""b""1\n" "2:\n"
	       ".section .fixup,\"ax\"\n"
	       "3:	mov %3,%0\n"
	       "	xor""b"" %""b""1,%""b""1\n"
	       "	jmp 2b\n"
	       ".previous\n"
	       " .section __ex_table,\"a\"\n"
	       " " ".balign 4" " " "\n"
	       " " ".long" " " "1b" "," "3b" "\n"
	       " .previous\n"
	       : "=r" (ret), "=q"(*(u8 *)to)
	       : "m" ((*(struct __large_struct *)(from))), "i" (1), "0" (ret));
  return ret;
}
void romchecksum(const unsigned char *rom, unsigned char c)
{
  unsigned char sum;
  for (sum = 0;
       !__copy_from_user_inatomic(&(c), ( typeof(c) *)(rom++), sizeof(c));)
    sum += c;
}
