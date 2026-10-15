/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-O2 -fduplicate-switch-dispatch --param=case-values-threshold=1 --param=max-goto-duplication-insns=128 -fdump-rtl-compgotos-details" } */

/* The two paths converge on an attachable switch dispatch, but there is no
   back edge into it.  */

int
plain (int x, int path, volatile int *state)
{
  if (path)
    state[12] += path;
  else
    state[13] ^= x;

  switch (x)
    {
    case 0: state[0] += 1; break;
    case 1: state[1] ^= 2; break;
    case 2: state[2] -= 3; break;
    case 3: state[3] |= 4; break;
    case 4: state[4] &= 5; break;
    case 5: state[5] += 6; break;
    case 6: state[6] ^= 7; break;
    case 7: state[7] -= 8; break;
    case 8: state[8] |= 9; break;
    case 9: state[9] &= 10; break;
    default: state[10] ^= 11; break;
    }
  return state[11];
}

/* { dg-final { scan-rtl-dump-not "Duplicating dispatch" "compgotos" } } */
