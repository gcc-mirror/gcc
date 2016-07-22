// PR c++/71675 - __atomic_compare_exchange_n returns wrong type for typed enum
// { dg-do compile { target c++11 } }

template <class T>
void sink (T);

bool sink (bool);

template <class T>
bool test ()
{
  enum class E: T { };
  static E e;

  return sink (__atomic_compare_exchange_n (&e, &e, e, false, 0, 0));
}

void tests ()
{
  // __atomic_compare_exchange_n would fail to return bool when
  //   its arguments were one of the three character types.
  test<char>();
  test<signed char>();
  test<unsigned char>();

  test<short>();
  test<unsigned short>();

  test<int>();
  test<unsigned int>();

  test<long>();
  test<unsigned long>();

  test<long long>();
  test<unsigned long long>();
}
