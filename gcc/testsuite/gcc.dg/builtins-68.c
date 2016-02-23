/* PR middle-end/69780 - [4.9/5/6 Regression] ICE on
     __builtin_alloca_with_align with small alignment */
/* { dg-require-effective-target alloca } */
/* { dg-do compile } */
/* { dg-options "-Wno-long-long" } */

#define CHAR_BIT  __CHAR_BIT__
#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)
#define LONG_MAX  __LONG_MAX__
#define LLONG_MAX __LONG_LONG_MAX__

static void* p;

/* Verify that valid __builtin_alloca_with_align expressions are accepted.  */
void test_valid (int n)
{
  enum {
    A1   = CHAR_BIT *   1,
    A2   = CHAR_BIT *   2,
    A4   = CHAR_BIT *   4,
    A8   = CHAR_BIT *   8,
    A16  = CHAR_BIT *  16,
    A32  = CHAR_BIT *  32
  };

  /* Valid alignments are power of 2 positive multiples of CHAR_BIT.  */
  p =  __builtin_alloca_with_align (n, CHAR_BIT *  1);
  p =  __builtin_alloca_with_align (n, CHAR_BIT *  2);
  p =  __builtin_alloca_with_align (n, CHAR_BIT *  4);
  p =  __builtin_alloca_with_align (n, CHAR_BIT *  8);
  p =  __builtin_alloca_with_align (n, CHAR_BIT * 16);
  p =  __builtin_alloca_with_align (n, CHAR_BIT * 32);

  p =  __builtin_alloca_with_align (n, A1);
  p =  __builtin_alloca_with_align (n, A2);
  p =  __builtin_alloca_with_align (n, A4);
  p =  __builtin_alloca_with_align (n, A8);
  p =  __builtin_alloca_with_align (n, A16);
  p =  __builtin_alloca_with_align (n, A32);
}

/* Non-integer alignments must be rejected.  */
void test_arg2_non_int (int n)
{
  /* Verify the full text of the diagnostic just once.  */
  p =  __builtin_alloca_with_align (n, 0.0);         /* { dg-error "second argument to function .__builtin_alloca_with_align. must be a constant integer power of 2 between .8. and " } */

  /* Disable diagnostic complaining about converting void* to int that
     preempts the "constant integer expression" error.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wint-conversion"

  p =  __builtin_alloca_with_align (n, (void*)0);    /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, "");          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, L"");         /* { dg-error "must be a constant integer" } */

#pragma GCC diagnostic pop

}

/* Integer alignment that's not a constant expression must be rejected.  */
void test_arg2_non_const (int n, int a1)
{
  extern const int a2;
  static const int a3 = CHAR_BIT;
  static volatile const int a4 = CHAR_BIT;
  
  p =  __builtin_alloca_with_align (n, a1);       /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, a2);       /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, a3);       /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, a4);       /* { dg-error "must be a constant integer" } */
}

/* Constant integer alignment that's not a power of 2 positive multiple
   of CHAR_BIT less than (1LLU << 32) must be rejected.  */
void test_arg2_non_pow2 (int n)
{
  p =  __builtin_alloca_with_align (n, INT_MIN);     /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, -1);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, !1);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, !0);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  0);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  1);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  2);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  3);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  4);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  5);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  6);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  7);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n,  9);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 10);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 11);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 12);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 13);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 14);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 15);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 17);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 31);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 33);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 63);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 65);          /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, INT_MAX);     /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, ~0U);         /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, LONG_MAX);    /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, ~0LU);        /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, 1LLU << 34);  /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, LLONG_MAX);   /* { dg-error "must be a constant integer" } */
  p =  __builtin_alloca_with_align (n, ~0LLU);       /* { dg-error "must be a constant integer" } */
}
