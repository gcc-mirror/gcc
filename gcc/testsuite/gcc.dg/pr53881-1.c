/* { dg-do compile } */
/* { dg-options "-O2" } */

int a, b;
void
fn1 ()
{
  int c;
  switch (a)
    {
    case 8:
      c = 0;
      goto Label;
    case 0:
    case 1:
Label:
      break;
    default:
      b = 0;
    }
}

