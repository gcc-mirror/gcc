/* PR target/13380.
   On m32r, the condition code register, (reg:SI 17), was replaced with
   a pseudo reg, which would cause an unrecognized insn.  */

void
foo (unsigned int a, unsigned int b)
{
  if (a > b)
    {
      while (a)
	{
	  switch (b)
	    {
	    default:
	      a = 0;
	    case 2:
	      a = 0;
	    case 1:
	      a = 0;
	    case 0:
	      ;
	    }
	}
    }
}
