/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-fpic" { target fpic } } */

struct S { char a; long long b; int c; };
enum E { E0, E1 = sizeof (struct S) + 15 };
int v[42];
void foo (void) {}

asm ("# %0 %1 %2 %cc3 %cc4 %% %="
     :: "i" (sizeof (struct S)),
	"i" (__builtin_offsetof (struct S, c)),
	"i" (E1),
	"-s" (foo),
	"-i" (v));
