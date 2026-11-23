/* PR target/128960 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mstackrealign" } */

typedef float v4sf __attribute__ ((vector_size (16)));

void sink (v4sf *);

__attribute__((target ("bmi2")))
void
with_bmi2 (void)
{
  v4sf v = { 0, 0, 0, 0 };
  sink (&v);
}

void
without_bmi2 (void)
{
  v4sf v = { 0, 0, 0, 0 };
  sink (&v);
}

/* Both functions must realign the stack in the prologue.  */
/* { dg-final { scan-assembler-times "and\[lq\]\[\t \]+\\$-16, %\[er\]sp" 2 } } */
