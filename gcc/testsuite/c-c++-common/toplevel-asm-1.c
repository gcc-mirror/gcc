/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-fno-pie" { target pie } } */

struct S { char a; long long b; int c; };
enum E { E0, E1 = sizeof (struct S) + 15 };
int v[42];
void foo (void) {}

/* Not all targets can use %cN even in non-pic code.  */
#if defined(__riscv) || defined(__loongarch__)
asm ("# %0 %1 %2 %cc3 %cc4 %5 %% %="
#else
asm ("# %0 %1 %2 %c3 %c4 %5 %% %="
#endif
     :: "i" (sizeof (struct S)),
	"i" (__builtin_offsetof (struct S, c)),
	"i" (E1),
	"s" (foo),
	"i" (v),
/* Not all targets can satisfy "m" even in non-pic code.  */
#if !defined(__i386__) && !defined(__x86_64__)
	"s" (v));
#else
	"m" (v));
asm ("# %0 %1"
     : "=m" (v[16])
     : "m" (v[41]));
#endif
