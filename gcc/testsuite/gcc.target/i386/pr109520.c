/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-O0" } */

int
foo (int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8,
     int a9, int a10, int a11, int a12, int a13, int a14, int a15, int a16)
{
  register int v0 asm ("rax") = a3;
  register int v1 asm ("rbx") = a4;
  register int v2 asm ("rcx") = a5;
  register int v3 asm ("rdx") = a6;
  register int v4 asm ("rsi") = a7;
  register int v5 asm ("rdi") = a8;
  register int v6 asm ("r8") = a9;
  register int v7 asm ("r9") = a10;
  register int v8 asm ("r10") = a11;
  register int v9 asm ("r11") = a12;
  register int v10 asm ("r12") = a13;
  register int v11 asm ("r13") = a14;
  register int v12 asm ("r14") = a15;
  register int v13 asm ("r15") = a16;
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
  asm goto ("": "=r" (x) : : : lab); /* { dg-error "operand has impossible constraints" } */
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
