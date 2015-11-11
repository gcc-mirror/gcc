/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp2" } */

extern int nc;
void ff (unsigned long long);

void
f (void)
{
  unsigned char resp[1024];
  int c;
  int bl = 0;
  unsigned long long *dwords = (unsigned long long *) (resp + 5);
  for (c = 0; c < nc; c++)
    {
      /* PR middle-end/68234, this signed division should be optimized into
	 right shift as vrp pass should deduct range info of 'bl' falls into
	 positive number.  */
      ff (dwords[bl / 64]);
      bl++;
    }
}

/* { dg-final { scan-tree-dump ">> 6" "vrp2" } } */
