/* PR target/29978 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

void g ();

void
f (long long v)
{
  if (v > 0xfffffffffLL)
    g ();
  g ();
}

/* Verify there are no redundant jumps jl .L2; jle .L2 */
/* { dg-final { scan-assembler-not "jl\[^e\]*\\.L" { target ilp32 } } } */
