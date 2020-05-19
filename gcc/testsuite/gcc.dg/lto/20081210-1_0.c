typedef __UINTPTR_TYPE__ uintptr_t;

extern void srand (uintptr_t);

inline void __attribute__ ((gnu_inline))
foo (uintptr_t seed)
{
 srand (seed * seed);
}

int
main ()
{
 foo (0);
 srand ((uintptr_t) (&foo));
 return 0;
}
