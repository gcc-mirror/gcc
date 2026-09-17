/* { dg-do run } */
/* { dg-options "-O2" } */

/* Y < W and Y < Q say nothing at all about Q versus W.  Composing the two
   relations in the wrong operand order produces Q > W, which is wrong code:
   the else arm below gets deleted.  */

int __attribute__((noipa))
f1 (int q, int w, int y)
{
  if (y < w)
    if (y < q)
      {
	if (q > w)
	  return 1;
	else
	  return 2;
      }
  return 0;
}

/* The mirror image, reached from the other end of the search.  */
int __attribute__((noipa))
f2 (int q, int w, int y)
{
  if (w > y)
    if (q > y)
      {
	if (w < q)
	  return 1;
	else
	  return 2;
      }
  return 0;
}

int
main ()
{
  if (f1 (1, 2, 0) != 2)
    __builtin_abort ();
  if (f1 (2, 1, 0) != 1)
    __builtin_abort ();
  if (f2 (2, 1, 0) != 1)
    __builtin_abort ();
  if (f2 (1, 2, 0) != 2)
    __builtin_abort ();
  return 0;
}
