/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

/* We should thread the if (exp == 2) conditional on the
   the path from inside the if (x) THEN arm.  It is the only
   jump threading opportunity in this code.  */
   
/* { dg-final { scan-tree-dump-times "Threaded" 1 "vrp1" } } */


extern void abort (void) __attribute__ ((__nothrow__, __leaf__))
  __attribute__ ((__noreturn__));

int x;


int code;
void
do_jump (int exp)
{
  switch (code)
    {
    case 4:
      if ((exp) == 1)
	goto normal;
      if (x)
	{
	  if (exp != 0)
	    abort ();
	}
      if ((exp) == 2)
	goto normal;
    case 3:
	abort ();
    }
  normal:
      ;
}
