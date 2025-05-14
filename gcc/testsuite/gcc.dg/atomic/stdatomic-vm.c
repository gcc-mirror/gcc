/* Test atomic operations on expressions of variably modified type
   with side effects.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);

int s = 5;

int count = 0;

int
func (void)
{
  count++;
  return 0;
}

int
main (void)
{
  int vla[s][s];
  int (*_Atomic p)[s] = &vla[0];
  int (*b)[s] = kill_dependency (++p);
  if (b != &vla[1] || p != &vla[1])
    abort ();
  int (*_Atomic *q)[s] = &p;
  atomic_store_explicit (q + func (), &vla[0], memory_order_seq_cst);
  if (count != 1)
    abort ();
  atomic_store (q + func (), &vla[0]);
  if (count != 2)
    abort ();
  (void) atomic_load_explicit (q + func (), memory_order_seq_cst);
  if (count != 3)
    abort ();
  (void) atomic_load (q + func ());
  if (count != 4)
    abort ();
  (void) atomic_exchange_explicit (q + func (), &vla[0], memory_order_seq_cst);
  if (count != 5)
    abort ();
  (void) atomic_exchange (q + func (), &vla[0]);
  if (count != 6)
    abort ();
  int vla2[s][s];
  int (*p2)[s] = &vla2[0];
  int (**qna)[s] = &p2;
  (void) atomic_compare_exchange_strong_explicit (q + func (), qna, &vla[0],
						  memory_order_seq_cst,
						  memory_order_seq_cst);
  if (count != 7)
    abort ();
  (void) atomic_compare_exchange_strong (q + func (), qna, &vla[0]);
  if (count != 8)
    abort ();
  (void) atomic_compare_exchange_weak_explicit (q + func (), qna, &vla[0],
						memory_order_seq_cst,
						memory_order_seq_cst);
  if (count != 9)
    abort ();
  (void) atomic_compare_exchange_weak (q + func (), qna, &vla[0]);
  if (count != 10)
    abort ();
  return 0;
}
