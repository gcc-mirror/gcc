/* { dg-do link } */
/* { dg-options "-fno-allow-store-data-races -O2" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include <stdlib.h>

#include "simulate-thread.h"

/* Test that speculative stores do not happen for --param
   allow-store-data-races=0.  */

int count, insns;

struct obj {
    int data;
    struct obj *next;
} *q;

void simulate_thread_other_threads ()
{
  ++insns;
  ++count;
}

int simulate_thread_step_verify ()
{
  return 0;
}

int simulate_thread_final_verify ()
{
  /* If count != insns, someone must have cached `count' and stored a
     racy value into it.  */
  if (count != insns)
    {
      printf("FAIL: count was incorrectly cached\n");
      return 1;
    }
  return 0;
}

/* Test that `count' is not written to unless p->data > 0.  */

__attribute__((noinline))
void simulate_thread_main()
{
  struct obj *p;
  for (p = q; p; p = p->next)
    if (p->data > 0)
      count++;
}

struct obj *
insert(struct obj *head, int data)
{
  struct obj *t = (struct obj *) malloc (sizeof (struct obj));
  t->next = head;
  t->data = data;
  return t;
}

int main()
{
  q = insert (0, 0);
  q = insert (q, 0);
  q = insert (q, 0);
  q = insert (q, 0);
  q = insert (q, 0);

  simulate_thread_main ();
  simulate_thread_done ();
  return 0;
}
