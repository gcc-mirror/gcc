/* Verify that __builtin_has_attribute detects attributes aligned
   and packed in various forms of array dereferencing and indirection
   expressions correspondingly to __alignof__.
   { dg-do compile }
   { dg-options "-Wall -Wno-unused -ftrack-macro-expansion=0" } */

#define ATTR(...) __attribute__ ((__VA_ARGS__))
#define ALIGN(N)  ATTR (aligned (N))

#define Assert(expr) typedef int _Assert [1 - 2 * !(expr)]

/* Verify that  __builtin_has_attribute (EXPR, align (ALIGN)) returns
   the EXPECTed result.  When EXPECT is true, verify that the EXPression
   has the expected ALIGNment.    */
#define A3(expect, expr, align) do {					\
    Assert (!expect || __alignof__ (expr) == align);			\
    Assert (expect == __builtin_has_attribute (expr, aligned (align))); \
  } while (0)

#define A(expect, expr)							\
  Assert (expect == __builtin_has_attribute (expr, aligned))		\

enum { PA = __alignof__ (void*) };

/* Define pointer to pointer types, with different alignments
   at each level of indirection.  */
typedef struct S8 { char a[8]; }   S8;
typedef ALIGN (8)  S8              I8;
typedef ALIGN (16) I8             *P16_I8;
typedef            P16_I8         *P_P16_I8;
typedef ALIGN (32) P_P16_I8       *P32_P_P16_I8;
typedef            P32_P_P16_I8   *P_P32_P_P16_I8;
typedef ALIGN (64) P_P32_P_P16_I8 *P64_P_P32_P_P16_I8;

Assert ( 8 == __alignof__ (I8));
Assert (16 == __alignof__ (P16_I8));
Assert (PA == __alignof__ (P_P16_I8));
Assert (32 == __alignof__ (P32_P_P16_I8));
Assert (PA == __alignof__ (P_P32_P_P16_I8));
Assert (64 == __alignof__ (P64_P_P32_P_P16_I8));


/* Similar to the pointer of pointers above, define array of array
   types, with different alignments at each level of indirection.  */
typedef struct S64 { char a[64]; } S64;
typedef ALIGN (64) S64             I64;
typedef ALIGN (32) I64             A32_I64[3];
typedef            A32_I64         A_A32_I64[5];
typedef ALIGN (16) A_A32_I64       A16_A_A32_I64[7];
typedef            A16_A_A32_I64   A_A16_A_A32_I64[11];
typedef ALIGN (8)  A_A16_A_A32_I64 A8_A_A16_A_A32_I64[13];

Assert (64 == __alignof__ (I64));
Assert (32 == __alignof__ (A32_I64));
/* With no explicit alignment, an array of overaligned elements
   is considered to have the alignment of its elements.  */
Assert (32 == __alignof__ (A_A32_I64));
Assert (16 == __alignof__ (A16_A_A32_I64));
Assert (16 == __alignof__ (A_A16_A_A32_I64));
Assert ( 8 == __alignof__ (A8_A_A16_A_A32_I64));


void test_arrays (void)
{
  /* Verify that the aligned attribute on each of the composite types
     is detected corresponding to the result of __alignof__.  */
  A (1, (*(A8_A_A16_A_A32_I64*)0));
  A3 (1, (*(A8_A_A16_A_A32_I64*)0), 8);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0], 8);
  /* GCC propagates the user-align bit from element types to their
     arrays but it doesn't propagate the attribute itself.  The built-in
     considers both the  bit and the attribute so it succeeds below even
     though the referenced type isn't declared with the attribute.  */
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0], 8);
  A3 (1, (*(A8_A_A16_A_A32_I64*)0)[0], 16);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0], 32);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1], 8);
  A3 (1, (*(A8_A_A16_A_A32_I64*)0)[0][1], 16);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1], 32);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1][2], 16);
  A3 (1, (*(A8_A_A16_A_A32_I64*)0)[0][1][2], 32);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1][2], 64);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1][2][3], 16);
  A3 (1, (*(A8_A_A16_A_A32_I64*)0)[0][1][2][3], 32);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1][2][3], 64);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1][2][3][4], 32);
  A3 (1, (*(A8_A_A16_A_A32_I64*)0)[0][1][2][3][4], 64);
  A3 (0, (*(A8_A_A16_A_A32_I64*)0)[0][1][2][3][4], 128);

  A8_A_A16_A_A32_I64 a;
  A3 (0, a[0], 8);
  A3 (1, a[0], 16);
  A3 (0, a[0], 32);
  A3 (0, a[0][1], 8);
  A3 (1, a[0][1], 16);
  A3 (0, a[0][1], 32);
  A3 (0, a[0][1][2], 16);
  A3 (1, a[0][1][2], 32);
  A3 (0, a[0][1][2], 64);
  A3 (0, a[0][1][2][3], 16);
  A3 (1, a[0][1][2][3], 32);
  A3 (0, a[0][1][2][3], 64);
  A3 (0, a[0][1][2][3][4], 32);
  A3 (1, a[0][1][2][3][4], 64);
  A3 (0, a[0][1][2][3][4], 128);
}

void test_pointers (void)
{
  /* Verify that the aligned attribute on each of the composite pointer
     types is detected corresponding to the result of __alignof__.  */
  A (1, I8);
  A3 (0, I8, 4);
  A3 (1, I8, 8);

  A (1, P16_I8);
  A3 (0, P16_I8, 8);
  A3 (1, P16_I8, 16);

  A (0, P_P16_I8);
  A3 (0, P_P16_I8, 8);
  A3 (0, P_P16_I8, 16);

  A (1, P32_P_P16_I8);
  A3 (0, P32_P_P16_I8, 8);
  A3 (0, P32_P_P16_I8, 16);
  A3 (1, P32_P_P16_I8, 32);

  A (0, P_P32_P_P16_I8);

  A (1, P64_P_P32_P_P16_I8);
  A3 (0, P64_P_P32_P_P16_I8, 8);
  A3 (0, P64_P_P32_P_P16_I8, 16);
  A3 (0, P64_P_P32_P_P16_I8, 32);
  A3 (1, P64_P_P32_P_P16_I8, 64);


  /* Verify that the attribute on each of the composite types is detected
     in the type of each of the indirection expressions.  */
  A (1, *(P16_I8)0);
  A3 (1, *(P16_I8)0, 8);
  A3 (0, *(P16_I8)0, 16);

  A (1, *(P_P16_I8)0);
  A3 (0, *(P_P16_I8)0, 8);
  A3 (1, *(P_P16_I8)0, 16);

  A (0, *(P32_P_P16_I8)0);
  A3 (0, *(P32_P_P16_I8)0, 8);
  A3 (0, *(P32_P_P16_I8)0, 16);
  A3 (0, *(P32_P_P16_I8)0, 32);

  A (1, *(P_P32_P_P16_I8)0);
  A3 (1, *(P_P32_P_P16_I8)0, 32);

  A (0, *(P64_P_P32_P_P16_I8)0);

  /* Verify that the attribute on each of the composite types is detected
     in the type of each of the subscipting expressions.  */
  A (1, ((P16_I8)0)[0]);
  A3 (1, ((P16_I8)0)[1], 8);
  A3 (0, ((P16_I8)0)[2], 16);

  A (1, ((P_P16_I8)0)[3]);
  A3 (0, ((P_P16_I8)0)[4], 8);
  A3 (1, ((P_P16_I8)0)[5], 16);

  A (0, ((P32_P_P16_I8)0)[6]);
  A3 (0, ((P32_P_P16_I8)0)[7], 8);
  A3 (0, ((P32_P_P16_I8)0)[8], 16);
  A3 (0, ((P32_P_P16_I8)0)[9], 32);

  A (1, ((P_P32_P_P16_I8)0)[10]);
  A3 (1, ((P_P32_P_P16_I8)0)[11], 32);

  A (0, ((P64_P_P32_P_P16_I8)0)[12]);


  /* Verify that the attribute on each of the composite types is detected
     in the type of each of the subscipting expression involving variables.  */

  I8                   i8;
  P16_I8               p16_i8 = &i8;
  P_P16_I8             p_p16_i8 = &p16_i8;
  P32_P_P16_I8         p32_p_p16_i8 = &p_p16_i8;
  P_P32_P_P16_I8       p_p32_p_p16_i8 = &p32_p_p16_i8;
  P64_P_P32_P_P16_I8   p64_p_p32_p_p16_i8 = &p_p32_p_p16_i8;

  A (1, p16_i8[0]);
  A3 (1, p16_i8[1], 8);
  A3 (0, p16_i8[2], 16);

  A (1, p_p16_i8[3]);
  A3 (0, p_p16_i8[4], 8);
  A3 (1, p_p16_i8[5], 16);

  A (0, p32_p_p16_i8[6]);
  A3 (0, p32_p_p16_i8[7], 8);
  A3 (0, p32_p_p16_i8[8], 16);
  A3 (0, p32_p_p16_i8[9], 32);

  A (1, p_p32_p_p16_i8[10]);
  A3 (1, p_p32_p_p16_i8[11], 32);


  A (1, p_p16_i8[0][1]);
  A3 (1, p_p16_i8[1][2], 8);
  A3 (0, p_p16_i8[2][3], 16);


  A (0, p64_p_p32_p_p16_i8[0]);

  A (1, p64_p_p32_p_p16_i8[0][1]);
  A3 (0, p64_p_p32_p_p16_i8[0][2], 16);
  A3 (1, p64_p_p32_p_p16_i8[0][3], 32);
  A3 (0, p64_p_p32_p_p16_i8[0][4], 64);

  A (0, p64_p_p32_p_p16_i8[0][1][2]);

  A (1, p64_p_p32_p_p16_i8[0][1][2][3]);
  A3 (0, p64_p_p32_p_p16_i8[0][1][2][4], 8);
  A3 (1, p64_p_p32_p_p16_i8[0][1][2][4], 16);
  A3 (0, p64_p_p32_p_p16_i8[0][1][2][4], 32);

  A (1, p64_p_p32_p_p16_i8[0][1][2][3][4]);
  A3 (1, p64_p_p32_p_p16_i8[0][1][2][3][5], 8);
  A3 (0, p64_p_p32_p_p16_i8[0][1][2][4][6], 16);


  /* Same as above but using the indirection expression.  */
  A (0, *p64_p_p32_p_p16_i8);

  A (1, **p64_p_p32_p_p16_i8);
  A3 (0, **p64_p_p32_p_p16_i8, 16);
  A3 (1, **p64_p_p32_p_p16_i8, 32);
  A3 (0, **p64_p_p32_p_p16_i8, 64);

  A (0, ***p64_p_p32_p_p16_i8);

  A (1, ****p64_p_p32_p_p16_i8);
  A3 (0, ****p64_p_p32_p_p16_i8, 8);
  A3 (1, ****p64_p_p32_p_p16_i8, 16);
  A3 (0, ****p64_p_p32_p_p16_i8, 32);

  A (1, *****p64_p_p32_p_p16_i8);
  A3 (1, *****p64_p_p32_p_p16_i8, 8);
  A3 (0, *****p64_p_p32_p_p16_i8, 16);
}


S8 f_S8 (void);
I8 f_I8 (void);
P16_I8 f_P16_I8 (void);
P_P16_I8 f_P_P16_I8 (void);
P32_P_P16_I8 f_P32_P_P16_I8 (void);
P_P32_P_P16_I8 f_P_P32_P_P16_I8 (void);
P64_P_P32_P_P16_I8 f_P64_P_P32_P_P16_I8 (void);

void test_function_call (void)
{
  /* Verify that the aligned attribute on each of the composite pointer
     types returned by the functions is detected corresponding to
     the result of __alignof__.  */

  A (0, f_S8 ());

  A (1, f_I8 ());
  A3 (1, f_I8 (), 8);
  A3 (0, f_I8 (), 16);

  A (1, f_P16_I8 ());
  A3 (0, f_P16_I8 (), 8);
  A3 (1, f_P16_I8 (), 16);
  A3 (0, f_P16_I8 (), 32);

  A (1, *f_P16_I8 ());
  A3 (1, *f_P16_I8 (), 8);
  A3 (0, *f_P16_I8 (), 16);

  A (0, f_P_P16_I8 ());

  A (1, *f_P_P16_I8 ());
  A3 (0, *f_P_P16_I8 (), 8);
  A3 (1, *f_P_P16_I8 (), 16);
  A3 (0, *f_P_P16_I8 (), 32);

  A (1, **f_P_P16_I8 ());
  A3 (1, **f_P_P16_I8 (), 8);
  A3 (0, **f_P_P16_I8 (), 16);
  A3 (0, **f_P_P16_I8 (), 32);
}


void test_compound_literal (void)
{
  A (0, (S8){ });

  A (1, (I8){ });
  A3 (1, (I8){ }, 8);
  A3 (0, (I8){ }, 16);

  A (1, (I64){ });
  A3 (0, (I64){ }, 8);
  A3 (0, (I64){ }, 16);
  A3 (0, (I64){ }, 32);
  A3 (1, (I64){ }, 64);

  A (1, (A32_I64){ 0 });
  A3 (0, (A32_I64){ 0 }, 8);
  A3 (0, (A32_I64){ 0 }, 16);
  A3 (1, (A32_I64){ 0 }, 32);
  A3 (0, (A32_I64){ 0 }, 64);

  A (1, ((A32_I64){ 0 })[0]);
  A3 (0, ((A32_I64){ 0 })[0], 8);
  A3 (0, ((A32_I64){ 0 })[0], 16);
  A3 (0, ((A32_I64){ 0 })[0], 32);
  A3 (1, ((A32_I64){ 0 })[0], 64);
}


void test_ternary_expression (int i)
{
  A (0, (0 ? (S8){ } : (S8){ }));

  A (1, (1 ? (I8){ } : (I8){ }));
  A3 (1, (2 ? (I8){ } : (I8){ }), 8);
  A3 (0, (3 ? (I8){ } : (I8){ }), 16);

  A (1, (4 ? (I64){ } : (I64){ }));
  A3 (0, (5 ? (I64){ } : (I64){ }), 8);
  A3 (0, (6 ? (I64){ } : (I64){ }), 16);
  A3 (0, (7 ? (I64){ } : (I64){ }), 32);
  A3 (1, (8 ? (I64){ } : (I64){ }), 64);

#if !__cplusplus
  /* Suppress -Wc++-compat warning: converting an array compound literal
     to a pointer is ill-formed in C++  */
# pragma GCC diagnostic ignored "-Wc++-compat"

  A (0, (9 ? (A32_I64){ } : (A32_I64){ })); 
  A3 (0, (i ? (A32_I64){ } : (A32_I64){ }), 8);
  A3 (0, (i++ ? (A32_I64){ } : (A32_I64){ }), 16);
  A3 (0, (++i ? (A32_I64){ } : (A32_I64){ }), 32);
  A3 (0, (!i ? (A32_I64){ } : (A32_I64){ }), 64);

  A (1, (0 ? (A32_I64){ } : (A32_I64){ })[0]);
  A3 (0, (1 ? (A32_I64){ } : (A32_I64){ })[1], 8);
  A3 (0, (2 ? (A32_I64){ } : (A32_I64){ })[2], 16);
  A3 (0, (3 ? (A32_I64){ } : (A32_I64){ })[3], 32);
  A3 (1, (3 ? (A32_I64){ } : (A32_I64){ })[i], 64);
#endif
}


void test_comma_expression (int i)
{
#if __cplusplus
  /* In C++, the type of the comma expressions whose operand is an array
     is the array itself with any attributes it was defined with.  */
# define R 1
#else
  /* In C, the type of the comma expressions whose operand is an array
     is a pointer type that does not include any attributes the array
     was defined with.  */
# define R 0
/* Suppress -Wc++-compat warning: converting an array compound literal
   to a pointer is ill-formed in C++
   G++ accepts the conversion in unevaluated contexts without a warning.  */
# pragma GCC diagnostic ignored "-Wc++-compat"
#endif

  A (0, (0, (S8){ }));

  A (1, (0, (I8){ }));
  A3 (1, (1, (I8){ }), 8);
  A3 (0, (2, (I8){ }), 16);

  A (1, (3, (I64){ }));
  A3 (0, (4, (I64){ }), 8);
  A3 (0, (5, (I64){ }), 16);
  A3 (0, (6, (I64){ }), 32);
  A3 (1, (7, (I64){ }), 64);

  A (R, (8, (A32_I64){ }));
  A3 (0, (9, (A32_I64){ }), 8);
  A3 (0, ((void)0, (A32_I64){ }), 16);
  A3 (R, ((I64){ },(A32_I64){ }), 32);
  A3 (0, (0, (A32_I64){ }), 64);

  A (1, (1, ((A32_I64){ })[0]));
  A3 (0, (2, ((A32_I64){ })[0]), 8);
  A3 (0, (i++, ((A32_I64){ })[0]), 16);
  A3 (0, (++i, ((A32_I64){ })[0]), 32);
  A3 (1, (i = 0, ((A32_I64){ })[0]), 64);
}
