#if defined(_LP64)
typedef unsigned long int uintptr_t;
#elif defined (_WIN64)
typedef unsigned long long int uintptr_t;
#else
typedef unsigned int uintptr_t;
#endif

extern void srand (uintptr_t);

inline void
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
