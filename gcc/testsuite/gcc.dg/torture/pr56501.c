/* { dg-do compile } */
/* { dg-options "-w" } */

int a;
void try_help () __attribute__ ((__noreturn__));
void try_help ()
{
}

int main ()
{
  switch (a)
    {
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
	  break;
      default:
	  try_help ();
    }
}
