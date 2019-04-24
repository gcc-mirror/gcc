/* { dg-do run } */

extern "C" void abort (void);
int
main (void)
{
  int i = -1; 
  switch ((signed char) i)
    {
      case 255: /* { dg-warning "exceeds maximum value" } */
	abort ();
      default:
	break;
    }
}

