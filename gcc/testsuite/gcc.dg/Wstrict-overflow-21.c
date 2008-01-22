/* PR 32102: -Wall stomps on -Wstrict-overflow */
/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=2 -Wall" } */
int
foo ()
{
  int i, bits;
  for (i = 1, bits = 1; i > 0; i += i) /* { dg-warning "assuming signed overflow does not occur" "correct warning" } */
    ++bits;
  return bits;
}

