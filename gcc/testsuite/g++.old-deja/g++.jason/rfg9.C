int cond;
int i;
int *ip;

void
test ()
{
  cond ? i : ip;	/* ERROR - pointer/integer mismatch */
}
