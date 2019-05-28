/* PR ipa/89009 */
/* { dg-do run } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpic -O2 -fno-inline" } */
/* { dg-require-visibility "" } */

void foo1() { __builtin_printf ("foo\n"); }
#pragma GCC visibility push(hidden)
void foo2() { __builtin_printf ("foo\n"); }
#pragma GCC visibility pop

int main() { foo2(); return 0; }

/* { dg-output "foo" } */
