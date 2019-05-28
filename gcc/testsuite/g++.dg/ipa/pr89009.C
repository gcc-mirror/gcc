/* PR ipa/89009 */
/* { dg-do run } */
/* { dg-options "-fvisibility=hidden -fpic -O2 -fno-inline" } */

#pragma GCC visibility push(default)
void foo1() { __builtin_printf ("foo\n"); }
#pragma GCC visibility pop
void foo2() { __builtin_printf ("foo\n"); }

int main() { foo2(); return 0; }

/* { dg-output "foo" } */
