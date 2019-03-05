/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdlib.h>

struct ubifs_budget_req {
  unsigned int fast:7;
  unsigned int new_ino_d:13;
};

int printf(const char *format, ...);

void __attribute__ ((noinline))
fff(struct ubifs_budget_req *req)
{
  if (req->new_ino_d & 7)
    abort ();
}

int main (void)
{
  struct ubifs_budget_req req = {
    .fast = 8,
    .new_ino_d = 0,
  };
  fff(&req);
  return 0;
}
