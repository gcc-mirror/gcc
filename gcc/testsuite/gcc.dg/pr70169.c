/* PR tree-optimization/70169 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fno-tree-dce" } */
/* { dg-skip-if "Program and data reside in different address spaces" { "avr-*-*" } } */
/* { dg-require-effective-target label_values } */

int printf (const char *, ...); 

void
foo ()
{
  unsigned char *p = (unsigned char *) &printf;
  for (;;)
    (*p)++;
}

void
bar (int x)
{
  unsigned char *p = (unsigned char *) &printf;
  int i;
  for (i = 0; i < x; i++)
    (*p)++;
}

void
baz (int x, int y)
{
  unsigned char *p = (unsigned char *) &&lab;
  int i;
  if (y)
    {
      for (i = 0; i < x; i++)
	(*p)++;
    }
  else
    {
     lab:
      asm volatile ("");
      foo ();
    }
}
