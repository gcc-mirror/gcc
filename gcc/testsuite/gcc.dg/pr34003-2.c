/* PR bootstrap/34003 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

extern void abort (void);

int seen = 0;

void foo (void)
{
  ++seen;
}

int main (void)
{
  extern int bar (void);
  if (bar () != 1 || seen != 1)
    abort ();
  return 0;
}
