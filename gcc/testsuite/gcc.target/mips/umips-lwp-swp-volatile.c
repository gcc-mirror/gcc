/* { dg-do compile } */
/* { dg-options "-mmicromips" } */

/* This test ensures that we do not generate microMIPS SWP or LWP
   instructions when any component of the accessed memory is volatile;
   they are unsafe for such since they might cause replay of partial
   accesses if interrupted by an exception.  */

static void set_csr (volatile void *p, int v)
{
  *(volatile int *) (p) = v;
}

static int get_csr (volatile void *p)
{
  return *(volatile int *) (p);
}

int main ()
{
  int i, q = 0, p = 0, r = 0;

  for (i = 0; i < 20; i++)
    {
      set_csr ((volatile void *) 0xbf0100a8, 0xffff0002);
      set_csr ((volatile void *) 0xbf0100a4, 0x80000008);
    }

  for (i = 0; i < 20; i++)
    {
      register int k, j;
      k = get_csr ((volatile void *) 0xbf0100b8);
      p += k;
      j = get_csr ((volatile void *) 0xbf0100b4);
      r += j;
      q = j + k;
    }
  return q + r + p;
}

/* { dg-final { scan-assembler-not "\tswp" } } */
/* { dg-final { scan-assembler-not "\tlwp" } } */
