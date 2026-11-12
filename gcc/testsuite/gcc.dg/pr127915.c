/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

/* last >= first and last <= 100, so last - first is [0, 100].  */
void
f1 (unsigned first, unsigned last)
{
  if (first > last || last > 100)
    __builtin_unreachable ();

  unsigned t = last - first;
  if (t > 100)
    __builtin_trap ();
}

/* last > first and last <= 100, so last - first is [1, 100].  */
void
f2 (unsigned first, unsigned last)
{
  if (first >= last || last > 100)
    __builtin_unreachable ();

  unsigned t = last - first;
  if (t == 0 || t > 100)
    __builtin_trap ();
}

/* first < last and last <= 100.  The subtraction always wraps, so
   first - last is [-100, -1].  */
void
f3 (unsigned first, unsigned last)
{
  if (first >= last || last > 100)
    __builtin_unreachable ();

  unsigned t = first - last;
  if (t < (unsigned) -100)
    __builtin_trap ();
}

/* first <= last and last <= 100.  As f3, except that first == last also
   produces zero, giving ~[1, -101].  */
void
f4 (unsigned first, unsigned last)
{
  if (first > last || last > 100)
    __builtin_unreachable ();

  unsigned t = first - last;
  if (t != 0 && t < (unsigned) -100)
    __builtin_trap ();
}

/* { dg-final { scan-tree-dump-not "__builtin_trap" "evrp" } } */
