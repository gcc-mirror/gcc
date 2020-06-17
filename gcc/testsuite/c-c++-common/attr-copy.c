/* PR c++/94346 - ICE due to handle_copy_attribute
   { dg-do compile }
   { dg-options "-Wall" } */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

#if __cplusplus > 199711L
#  define SA(expr) static_assert (expr, #expr)
#elif __cplusplus
#  define SA(expr)							\
  typedef __attribute__ ((unused)) char Assert[!(expr) ? -1 : 1]
#else
#  define SA(expr) _Static_assert (expr, #expr)
#endif

typedef struct ATTR (packed) A { ATTR (packed) unsigned bf: 1; } A;

int bar (void);

struct C
{
  char c;
  ATTR (copy ((bar (), ((struct A *)(0))[0]))) int i;
};

/* Verify the attribute has been copied.  */
SA (__builtin_offsetof (struct C, i) == 1);



/* Verify attribute copy can copy from the type a comma expression.  */
ATTR (alloc_size (1)) void* alloc1 (int);

ATTR (copy ((bar (), alloc1))) void* alloc2 (int, int);

ATTR (copy ((bar (), alloc1))) void alloc3 (int);  /* { dg-warning "'alloc_size' attribute ignored on a function returning 'void'" } */


typedef ATTR (alloc_size (1)) void* F (int);

ATTR (copy ((bar (), (F*)0))) void* alloc4 (int, int);

ATTR (copy ((bar (), (F*)0))) void alloc5 (int, int);  /* { dg-warning "'alloc_size' attribute ignored on a function returning 'void'" } */
