typedef int (*FPTR) (void);
FPTR a;

int
func ()
{
  int b = a ();
  return b;
}
