/* Source: PR 321 modified for test suite by Neil Booth 14 Jan 2001.  */

typedef unsigned long long uint64;
unsigned long pars;

uint64 b[32];
uint64 *r = b;

void alpha_ep_extbl_i_eq_0()
{
  unsigned int rb, ra, rc;

  rb  = (((unsigned long)(pars) >> 27)) & 0x1fUL;
  ra  = (((unsigned int)(pars) >> 5)) & 0x1fUL;
  rc  = (((unsigned int)(pars) >> 0)) & 0x1fUL;
  {
    uint64 temp = ((r[ra] >> ((r[rb] & 0x7) << 3)) & 0x00000000000000FFLL); 
    if (rc != 31) 
      r[rc] = temp;  
  }
}

int 
main(void)
{
  if (sizeof (uint64) == 8)
    {
      b[17] = 0x0000000000303882ULL; /* rb */
      b[2] = 0x534f4f4c494d000aULL; /* ra & rc */

      pars = 0x88000042;	/* 17, 2, 2 coded */
      alpha_ep_extbl_i_eq_0();

      if (b[2] != 0x4d)
	abort ();
    }

  exit (0);
}
