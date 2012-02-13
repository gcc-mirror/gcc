// PR c++/52215
// { dg-do compile }

enum E { ZERO };

int
main ()
{
  E e = ZERO;
  __atomic_compare_exchange_n (&e, &e, e, true, __ATOMIC_ACQ_REL,
			       __ATOMIC_RELAXED);
}
