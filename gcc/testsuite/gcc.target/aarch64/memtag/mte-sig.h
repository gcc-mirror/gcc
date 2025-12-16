#include <stdlib.h>
#include <signal.h>
#include <string.h>

void handler (int nSig)
{
  /* We hit the exception.  Return error.  */
  exit (1);
}


static void setHandler (void)
{
  signal (SIGSEGV, handler);
}
