/* PR middle-end/126084 */
/* { dg-do run { target bitint575 } } */
/* { dg-shouldfail "asan" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O2" } } */

_BitInt(135)
foo ()
{
  _BitInt(135) *p, *q;
  {
    _BitInt(135) a, b;
    p = &a;
    q = &b;
  }
  return *p + *q;
}

int
main ()
{
  volatile _BitInt(135) x = foo ();
}

/* { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" } */
/* { dg-output "READ of size .*" } */
/* { dg-output ".*'a' \\(line 11\\) <== Memory access at offset \[0-9\]* is inside this variable.*" } */
