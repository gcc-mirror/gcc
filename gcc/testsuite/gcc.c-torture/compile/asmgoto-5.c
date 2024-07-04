/* Test to generate output reload in asm goto on x86_64.  */
/* { dg-do compile { target asm_goto_with_outputs } } */
/* { dg-skip-if "no O0" { { i?86-*-* x86_64-*-* } && { ! ia32 } } { "-O0" } { "" } } */

#if defined __x86_64__
#define ASM(s) asm (s)
#else
#define ASM(s)
#endif

int
foo (int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8,
     int a9, int a10, int a11, int a12, int a13, int a14, int a15, int a16)
{
  register int v0 ASM ("rax") = a3;
  register int v1 ASM ("rbx") = a4;
  register int v2 ASM ("rcx") = a5;
  register int v3 ASM ("rdx") = a6;
  register int v4 ASM ("rsi") = a7;
  register int v5 ASM ("rdi") = a8;
  register int v6 ASM ("r8") = a9;
  register int v7 ASM ("r9") = a10;
  register int v8 ASM ("r10") = a11;
  register int v9 ASM ("r11") = a12;
  register int v10 ASM ("r12") = a13;
  register int v11 ASM ("r13") = a14;
  register int v12 ASM ("r14") = a15;
  register int v13 ASM ("r15") = a16;
  int x;
  
  v0 += a0;
  v1 += a1;
  v2 += a2;
  v0 |= a0;
  v1 |= a1;
  v2 |= a2;
  v0 ^= a0;
  v1 ^= a1;
  v2 ^= a2;
  v0 &= a0;
  v1 &= a1;
  v2 &= a2;
  asm goto ("": "=r" (x) : : : lab);
  a1 ^= a0;
  a2 = a1;
  a0 |= a2;
  a0 |= x;
 lab:
  v0 += x + a0 + a1 + a2;
  v1 -= a0 - a1 - a2;
  v2 |= a0 | a1 | a2;
  v3 |= a0 & a1 & a2;
  v4 ^= a0 ^ a1 ^ a2;
  return  v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + a0 + a1 + a2;
}

