/* Called by pr68078. */

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>

void
set_vm_limit (int vm_limit)
{
  struct rlimit rl;
  int r;

  r = getrlimit (RLIMIT_AS, &rl);
  if (r)
    {
      perror ("get_vm_limit");
      exit (1);
    }

  if (vm_limit >= rl.rlim_cur)
    return;

  rl.rlim_cur = vm_limit;
  r = setrlimit (RLIMIT_AS, &rl);
  if (r)
    {
      perror ("set_vm_limit");
      exit (1);
    }

  return;
}
