/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

extern void good (void);
extern void bad (void);

/* Switch simplification should remove 'case 2:' because 'i' will always
 * have its 0th bit set (odd). */

void bitmask_elimination_1(int i)
{
  i = i | 1;

  switch (i)
    {
    case 1:
      good ();
      break;

    // This case should be removed;
    case 2:
      bad ();
      break;

    case 3:
      good ();
      break;

    default:
      break;
    }
}

/* Switch simplification should remove 'case 20-28:' because 'i' will always
 * be a multiple of 16.  */
void bitmask_elimination_2 (int i)
{
  int masked_val = i & 0xF0; // This zeroes out the lower 4 bits of 'i'

  switch (masked_val)
  {
    case 0:
      good (); // Reachable.
      break;

    // This entire cased should be removed;
    case 20 ... 28:
      bad ();
      break;

    case 32:
      good (); // Reachable.
      break;

    default:
      good ();
      break;
  }
}
/* { dg-final { scan-tree-dump-not "bad" "evrp" } } */
