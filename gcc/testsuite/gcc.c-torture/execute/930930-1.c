long *wm_TR;
long *wm_HB;
long *wm_SPB;

long mem[100];

f (mr_TR, mr_SPB, mr_HB, reg1, reg2)
     long *mr_TR;
     long *mr_SPB;
     long *mr_HB;
     long *reg1;
     long *reg2;
{
  long *x = mr_TR;

  for (;;)
    {
      if (reg1 < reg2)
	goto out;
      if ((long *) *reg1 < mr_HB && (long *) *reg1 >= mr_SPB)
	*--mr_TR = *reg1;
      reg1--;
    }
 out:

  if (x != mr_TR)
    abort ();
}

main ()
{
  mem[99] = (long) mem;
  f (mem + 100, mem + 6, mem + 8, mem + 99, mem + 99);
  exit (0);
}
