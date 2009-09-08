/* Testcase by Martin Michlmayr <tbm@cyrius.com> */
/* { dg-do compile } */
/* { dg-require-effective-target vect_shift } */

int clamp_val (int i)
{
  return ~i >> 31;
}

typedef __PTRDIFF_TYPE__ intptr_t;

void _mix_some_samples (intptr_t buf, int *mix_buffer, int mix_size)
{
  int i;
  signed int *p = mix_buffer;
  for (i = mix_size ; i > 0; i--)
  {
    *((short *) buf) = clamp_val ((*p) + 0x800000);
    buf += 2;
    p++;
  }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
