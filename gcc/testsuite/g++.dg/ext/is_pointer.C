// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

SA(!__is_pointer(int));
SA(__is_pointer(int*));
SA(__is_pointer(int**));

SA(__is_pointer(const int*));
SA(__is_pointer(const int**));
SA(__is_pointer(int* const));
SA(__is_pointer(int** const));
SA(__is_pointer(int* const* const));

SA(__is_pointer(volatile int*));
SA(__is_pointer(volatile int**));
SA(__is_pointer(int* volatile));
SA(__is_pointer(int** volatile));
SA(__is_pointer(int* volatile* volatile));

SA(__is_pointer(const volatile int*));
SA(__is_pointer(const volatile int**));
SA(__is_pointer(const int* volatile));
SA(__is_pointer(volatile int* const));
SA(__is_pointer(int* const volatile));
SA(__is_pointer(const int** volatile));
SA(__is_pointer(volatile int** const));
SA(__is_pointer(int** const volatile));
SA(__is_pointer(int* const* const volatile));
SA(__is_pointer(int* volatile* const volatile));
SA(__is_pointer(int* const volatile* const volatile));

SA(!__is_pointer(int&));
SA(!__is_pointer(const int&));
SA(!__is_pointer(volatile int&));
SA(!__is_pointer(const volatile int&));

SA(!__is_pointer(int&&));
SA(!__is_pointer(const int&&));
SA(!__is_pointer(volatile int&&));
SA(!__is_pointer(const volatile int&&));

SA(!__is_pointer(int[3]));
SA(!__is_pointer(const int[3]));
SA(!__is_pointer(volatile int[3]));
SA(!__is_pointer(const volatile int[3]));

SA(!__is_pointer(int(int)));
SA(__is_pointer(int(*const)(int)));
SA(__is_pointer(int(*volatile)(int)));
SA(__is_pointer(int(*const volatile)(int)));
