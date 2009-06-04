/* { dg-do run } */

extern "C" void abort (void);
int
main (void)
{
  int i = -1; 
  switch ((signed char) i)
    {
      case 255: /* { dg-bogus "exceeds maximum value" "" { xfail *-*-* } } */
	abort ();
      default:
	break;
    }
}

