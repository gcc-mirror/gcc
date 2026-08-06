// { dg-do run }
//
/* More than 16 bytes, so S is returned in memory.  *p becomes the return slot
   of the call and GIMPLE keeps a single statement "*p_7(D) = h (k_8(D));",
   a call that can throw whose store operand infers p != 0.  */

struct S { int a[8]; };

__attribute__((noipa)) S
h (int k)
{
  if (k)
    throw 1;                    /* Thrown before anything is stored.  */
  S s = {};
  s.a[0] = 5;
  return s;
}

__attribute__((noipa)) int
f (S *p, int k)
{
  int r = 0;
  try
    {
      *p = h (k);               /* Infers p != 0, but only if it completes.  */
      r = 1;
    }
  catch (...)
    {
      if (p == 0)               /* Must not fold: the store never ran.  */
        r = 12;
      else
        r = 2;
    }
  return r;
}

S obj;

int
main (void)
{
  if (f (&obj, 0) != 1)         /* Normal path, p is &obj.  */
    __builtin_abort ();
  if (f (0, 1) != 12)           /* h throws, no store, p is null.  */
    __builtin_abort ();
  return 0;
}

