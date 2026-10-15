/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-additional-options "-fduplicate-switch-dispatch --param=max-goto-duplication-insns=128 --param=case-values-threshold=1" } */

/* The block before each original dispatch leaves the function, through a
   noreturn call in f1 and through a call and a return in f2.  Every
   predecessor of a dispatch takes a copy, so the original is deleted and its
   shared table has to move after one of the copies.  */

extern void fail (int) __attribute__ ((noreturn));
extern void note (int);

int
f1 (const unsigned char *pc, const unsigned char *pc2, int acc)
{
  if (!pc)
    fail (acc);
  for (;;)
    switch (*pc++)
      {
      case 0: goto second;
      case 1: acc += 3; break;
      case 2: acc *= 5; break;
      case 3: acc ^= 7; break;
      case 4: acc -= *pc++; break;
      case 5: acc += 11; break;
      default: __builtin_unreachable ();
      }
 second:
  if (!pc2)
    fail (acc + 1);
  for (;;)
    switch (*pc2++)
      {
      case 0: return acc;
      case 1: acc -= 3; break;
      case 2: acc *= 7; break;
      case 3: acc ^= 9; break;
      case 4: acc += *pc2++; break;
      case 5: acc -= 13; break;
      default: __builtin_unreachable ();
      }
}

int
f2 (const unsigned char *pc, const unsigned char *pc2, int acc)
{
  if (!pc)
    {
      note (acc);
      return -1;
    }
  for (;;)
    switch (*pc++)
      {
      case 0: goto second;
      case 1: acc += 3; break;
      case 2: acc *= 5; break;
      case 3: acc ^= 7; break;
      case 4: acc -= *pc++; break;
      case 5: acc += 11; break;
      default: __builtin_unreachable ();
      }
 second:
  if (!pc2)
    {
      note (acc + 1);
      return -2;
    }
  for (;;)
    switch (*pc2++)
      {
      case 0: return acc;
      case 1: acc -= 3; break;
      case 2: acc *= 7; break;
      case 3: acc ^= 9; break;
      case 4: acc += *pc2++; break;
      case 5: acc -= 13; break;
      default: __builtin_unreachable ();
      }
}
