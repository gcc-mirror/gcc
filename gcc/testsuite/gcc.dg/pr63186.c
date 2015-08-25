/* { dg-do link } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target label_values } */

void *a;
int b, c, d;

void
bar ()
{
  switch (c)
    {
    case 0:
    lab:
      __asm__ ("");
      return;
    default:
      break;
    }
  b = 0;
  d = 0;
  a = &&lab;
}

void
foo ()
{
  bar ();
}

int
main()
{
}
