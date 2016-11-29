/* Called by pr68078. */

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>

void
set_vm_limit (int vm_limit)
{
  struct rlimit rl = { vm_limit, RLIM_INFINITY };
  int r;

  r = setrlimit (RLIMIT_AS, &rl);
  if (r)
    {
      perror ("set_vm_limit");
      exit (1);
    }

  return;
}
