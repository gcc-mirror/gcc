// { dg-do assemble  }
int cond;
int i;
int *ip;

void
test ()
{
  cond ? i : ip;	/* { dg-error "" } pointer/integer mismatch */
}
