#include <stdlib.h>

/* Don't complain about leaks due to exiting from "main".  */

void main (void)
{
  void *p = malloc (1024);
}
