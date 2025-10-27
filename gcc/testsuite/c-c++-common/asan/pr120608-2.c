/* PR middle-end/120608 */
/* { dg-do run { target musttail } } */
/* { dg-options "-O2 -fsanitize=address" } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_stack_use_after_return=1" } */
/* { dg-shouldfail "asan" } */

__attribute__((noipa)) void
foo (int *x, int *y, int *z)
{
  ++x[0];
  ++y[0];
  ++z[0];
}

__attribute__((noipa)) void
bar (int *x, int *y, int *z)
{
  volatile int a = x[0] + y[0] + z[0];
}

__attribute__((noipa)) void
baz (int *x, int *y, int *z)
{
  (void) x; (void) y; (void) z;
  int a = 42, b = -42, c = 0;
  foo (&a, &b, &c);
  [[gnu::musttail]] return bar (&a, &b, &c);	/* { dg-warning "address of automatic variable 'a' passed to 'musttail' call argument" } */
}						/* { dg-warning "address of automatic variable 'b' passed to 'musttail' call argument" "" { target *-*-* } .-1 } */
						/* { dg-warning "address of automatic variable 'c' passed to 'musttail' call argument" "" { target *-*-* } .-2 } */

int
main ()
{
  baz (0, 0, 0);
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-return on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size .*" }
// { dg-output ".*'a' \\(line 25\\) <== Memory access at offset \[0-9\]* is inside this variable.*" }
