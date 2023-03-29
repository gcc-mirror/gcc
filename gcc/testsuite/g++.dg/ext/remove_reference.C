// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

SA(__is_same(__remove_reference(void), void));
SA(__is_same(__remove_reference(int*), int*));

SA(__is_same(__remove_reference(int&), int));
SA(__is_same(__remove_reference(const int&), const int));
SA(__is_same(__remove_reference(volatile int&), volatile int));
SA(__is_same(__remove_reference(const volatile int&), const volatile int));

SA(__is_same(__remove_reference(int&&), int));
SA(__is_same(__remove_reference(const int&&), const int));
SA(__is_same(__remove_reference(volatile int&&), volatile int));
SA(__is_same(__remove_reference(const volatile int&&), const volatile int));

SA(__is_same(__remove_reference(int[3]), int[3]));
SA(__is_same(__remove_reference(const int[3]), const int[3]));
SA(__is_same(__remove_reference(volatile int[3]), volatile int[3]));
SA(__is_same(__remove_reference(const volatile int[3]), const volatile int[3]));

SA(__is_same(__remove_reference(int(int)), int(int)));
SA(__is_same(__remove_reference(int(*const)(int)), int(*const)(int)));
SA(__is_same(__remove_reference(int(*volatile)(int)), int(*volatile)(int)));
SA(__is_same(__remove_reference(int(*const volatile)(int)), int(*const volatile)(int)));

template<class T> using const_non_ref_t = const __remove_reference(T);
SA(__is_same(const_non_ref_t<int>, const int));
SA(__is_same(const_non_ref_t<volatile int&>, const volatile int));
SA(__is_same(const_non_ref_t<int&>, const int));
SA(__is_same(const_non_ref_t<const int&>, const int));
