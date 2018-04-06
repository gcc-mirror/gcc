/* PR c/46921  lost side effect when struct initializer expr uses comma
      operator  */

/* { dg-do run } */
/* { dg-options "" } */

extern int printf(const char *, ...);
extern void abort (void);

typedef struct __uws_0 { } uw_unit;
uw_unit uw_unit_v = {};

struct __uws_1 
{
  struct __uws_0 __uwf_1;
  struct __uws_1* __uwf_2;
};

static int left_hand_eval = 0;

static void
foo (const char *s)
{
  ++left_hand_eval;
  printf("%s", s);
}

int
main ()
{
  struct __uws_1 tmp = {(foo("Inner\n"), uw_unit_v)};
  
  printf("Outer\n");
  /* left hand expression in comma operator initializer must always be
     evaluated if there are side effects.  */
  if (!left_hand_eval)
    abort ();
  
  return 0;
}
