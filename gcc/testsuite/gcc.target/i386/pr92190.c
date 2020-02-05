/* PR target/92190 */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-mabi=ms -O2 -mavx512f" } */

typedef char VC __attribute__((vector_size (16)));
typedef int VI __attribute__((vector_size (16 * sizeof 0)));
VC a;
VI b;
void bar (VI);
void baz (VC);

void
foo (void)
{
  VC k = a;
  VI n = b;
  bar (n);
  baz (k);
}
