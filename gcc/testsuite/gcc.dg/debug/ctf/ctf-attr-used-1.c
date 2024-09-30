/* Test CTF generation works well with ((used)) function attribute.

   This attribute, attached to a function, means that code must be emitted for
   the function even if it appears that the function is not referenced.  */

/* { dg-do compile } */
/* { dg-options "-O2 -gctf -dA" } */

/* These should be true for higher optimization levels.  */
/* { dg-final { scan-assembler-times "ascii \"keep_this.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"lose_this.0\"\[\t \]+\[^\n\]*ctf_string" 0 } } */

static int lose_this(int a)
{
    return a + 2;
}

__attribute__((used))
static int keep_this(double a)
{
    return a * 2;
}
