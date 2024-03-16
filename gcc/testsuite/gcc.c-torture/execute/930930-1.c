/* { dg-additional-options "-std=gnu89" } */

__extension__ typedef __PTRDIFF_TYPE__ ptr_t;
ptr_t *wm_TR;
ptr_t *wm_HB;
ptr_t *wm_SPB;

ptr_t mem[100];

f (mr_TR, mr_SPB, mr_HB, reg1, reg2)
     ptr_t *mr_TR;
     ptr_t *mr_SPB;
     ptr_t *mr_HB;
     ptr_t *reg1;
     ptr_t *reg2;
{
  ptr_t *x = mr_TR;

  for (;;)
    {
      if (reg1 < reg2)
	goto out;
      if ((ptr_t *) *reg1 < mr_HB && (ptr_t *) *reg1 >= mr_SPB)
	*--mr_TR = *reg1;
      reg1--;
    }
 out:

  if (x != mr_TR)
    abort ();
}

main ()
{
  mem[99] = (ptr_t) mem;
  f (mem + 100, mem + 6, mem + 8, mem + 99, mem + 99);
  exit (0);
}
