/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-additional-options "-fduplicate-switch-dispatch" } */

/* Gaps and shared case labels make table entries repeat, so the successors
   of a copy are a multiset.  A case target that is a plain forwarder makes
   cfg cleanup want to redirect an edge of the shared table.  */

int sink;

__attribute__((noipa)) int
run (const signed char *pc, int n)
{
  int acc = 0;

  for (;;)
    switch (*pc++)
      {
      case 3:
      case 5:
      case 7:
	acc += 2;
	break;
      case 4:
	goto fwd;
      case 6:
	acc *= 3;
	break;
      case 9:
	acc -= 1;
	break;
      case 12:
	acc ^= 0x55;
	break;
      case 17:
	return acc + n;
      default:
	acc += 1000;
	break;
      fwd:
	acc += 7;
	break;
      }
}

static const signed char prog[] = {
  3, 5, 7, 4, 6, 9, 12, 1, -8, 100, 4, 4, 6, 17
};

int
main (void)
{
  int acc = 0;

  acc += 2; acc += 2; acc += 2;		/* 3 5 7 */
  acc += 7;				/* 4 */
  acc *= 3;				/* 6 */
  acc -= 1;				/* 9 */
  acc ^= 0x55;				/* 12 */
  acc += 1000; acc += 1000; acc += 1000;	/* 1 -8 100 */
  acc += 7; acc += 7;			/* 4 4 */
  acc *= 3;				/* 6 */

  if (run (prog, 11) != acc + 11)
    __builtin_abort ();
  return 0;
}
