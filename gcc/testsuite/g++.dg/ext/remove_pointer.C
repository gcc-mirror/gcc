// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

SA(__is_same(__remove_pointer(int), int));
SA(__is_same(__remove_pointer(int*), int));
SA(__is_same(__remove_pointer(int**), int*));

SA(__is_same(__remove_pointer(const int*), const int));
SA(__is_same(__remove_pointer(const int**), const int*));
SA(__is_same(__remove_pointer(int* const), int));
SA(__is_same(__remove_pointer(int** const), int*));
SA(__is_same(__remove_pointer(int* const* const), int* const));

SA(__is_same(__remove_pointer(volatile int*), volatile int));
SA(__is_same(__remove_pointer(volatile int**), volatile int*));
SA(__is_same(__remove_pointer(int* volatile), int));
SA(__is_same(__remove_pointer(int** volatile), int*));
SA(__is_same(__remove_pointer(int* volatile* volatile), int* volatile));

SA(__is_same(__remove_pointer(const volatile int*), const volatile int));
SA(__is_same(__remove_pointer(const volatile int**), const volatile int*));
SA(__is_same(__remove_pointer(const int* volatile), const int));
SA(__is_same(__remove_pointer(volatile int* const), volatile int));
SA(__is_same(__remove_pointer(int* const volatile), int));
SA(__is_same(__remove_pointer(const int** volatile), const int*));
SA(__is_same(__remove_pointer(volatile int** const), volatile int*));
SA(__is_same(__remove_pointer(int** const volatile), int*));
SA(__is_same(__remove_pointer(int* const* const volatile), int* const));
SA(__is_same(__remove_pointer(int* volatile* const volatile), int* volatile));
SA(__is_same(__remove_pointer(int* const volatile* const volatile), int* const volatile));

SA(__is_same(__remove_pointer(int&), int&));
SA(__is_same(__remove_pointer(const int&), const int&));
SA(__is_same(__remove_pointer(volatile int&), volatile int&));
SA(__is_same(__remove_pointer(const volatile int&), const volatile int&));

SA(__is_same(__remove_pointer(int&&), int&&));
SA(__is_same(__remove_pointer(const int&&), const int&&));
SA(__is_same(__remove_pointer(volatile int&&), volatile int&&));
SA(__is_same(__remove_pointer(const volatile int&&), const volatile int&&));

SA(__is_same(__remove_pointer(int[3]), int[3]));
SA(__is_same(__remove_pointer(const int[3]), const int[3]));
SA(__is_same(__remove_pointer(volatile int[3]), volatile int[3]));
SA(__is_same(__remove_pointer(const volatile int[3]), const volatile int[3]));

SA(__is_same(__remove_pointer(int(int)), int(int)));
SA(__is_same(__remove_pointer(int(*const)(int)), int(int)));
SA(__is_same(__remove_pointer(int(*volatile)(int)), int(int)));
SA(__is_same(__remove_pointer(int(*const volatile)(int)), int(int)));
