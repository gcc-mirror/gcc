/* { dg-do run } */
/* { dg-options "-O1 -Wall" } */

#define choose __builtin_choose_expr

/* Check the type of __builtin_choose_expr between E1 and E2, both
   ways round and with both 0 and 1 as the condition.  */
#define ASSERT_COND_TYPE(E1, E2)				\
        do {							\
          typedef __typeof(E1) T1;				\
          typedef __typeof(E2) T2;				\
          typedef T1 **T1pp;					\
          typedef T2 **T2pp;					\
          typedef __typeof(choose (1, (E1), (E2))) T1a;		\
          typedef __typeof(choose (0, (E2), (E1))) T1b;		\
          typedef __typeof(choose (1, (E2), (E1))) T2a;		\
          typedef __typeof(choose (0, (E1), (E2))) T2b;		\
          typedef T1a **T1app;					\
          typedef T1b **T1bpp;					\
          typedef T2a **T2app;					\
          typedef T2b **T2bpp;					\
          T1pp t1 = 0;						\
          T2pp t2 = 0;						\
          T1app t1a = 0;					\
          T1bpp t1b = 0;					\
          T2app t2a = 0;					\
          T2bpp t2b = 0;					\
          t1 = t1a;						\
          t1 = t1b;						\
          t2 = t2a;						\
          t2 = t2b;						\
          (void) t1;						\
          (void) t2;						\
        } while (0)


extern void abort ();
extern void exit ();

void bad ()
{
  abort ();
}

void good ()
{
  exit (0);
}

int main (void)
{
  signed char sc1, sc2;
  void *v1;
  int i, j;
  double dd;
  float f;
  typedef void (*fpt)(void);
  fpt triple;
  struct S { int x, y; } pour, some, sugar;
  union u { int p; } united, nations;

  if (__builtin_choose_expr (0, 12, 0)
      || !__builtin_choose_expr (45, 5, 0)
      || !__builtin_choose_expr (45, 3, 0))
    abort ();

  ASSERT_COND_TYPE (sc1, sc2);
  ASSERT_COND_TYPE (v1, sc1);
  ASSERT_COND_TYPE (i, j);
  ASSERT_COND_TYPE (dd, main);
  ASSERT_COND_TYPE ((float)dd, i);
  ASSERT_COND_TYPE (4, f);
  ASSERT_COND_TYPE (triple, some);
  ASSERT_COND_TYPE (united, nations);
  ASSERT_COND_TYPE (nations, main);

  pour.y = 69;
  __builtin_choose_expr (0, bad (), sugar) = pour;
  if (sugar.y != 69)
    abort ();

  __builtin_choose_expr (sizeof (int), f, bad ()) = 3.5F;

  if (f != 3.5F)
    abort ();

  __builtin_choose_expr (1, good, bad)();

  exit (0);
}
