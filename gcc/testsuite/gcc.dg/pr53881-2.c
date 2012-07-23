/* { dg-do compile } */
/* { dg-options "-O2" } */

int a,b,c;
void
fn1 ()
{
  switch (a)
    {
    case 0:
    case 10:
      b=c;
out_bcon:
      break;
    case 3:
      goto out_bcon;
    }
}

