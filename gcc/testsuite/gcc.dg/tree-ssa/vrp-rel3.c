/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

/* Masking with 0xff makes C a partial equivalence of A, and unlike the cast
   case both names have the same type, so an ordinary comparison can relate
   the pair as well.  A partial equivalence and an ordering relation do not
   combine, but the partial equivalence must not hide the ordering either.  */

void keep (void);
void kill (void);

void
f1 (int a, int b)
{
  int c = a & 0xff;
  if (c > b)
    if (b > a)
      {
	if (c > a)
	  keep ();
	else
	  kill ();
      }
}

/* Same shape with a mask which is not a partial equivalence, as a control.  */
void
f2 (int a, int b)
{
  int c = a & 0xfe;
  if (c > b)
    if (b > a)
      {
	if (c > a)
	  keep ();
	else
	  kill ();
      }
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } } */
/* { dg-final { scan-tree-dump-times "keep" 2 "evrp" } } */
