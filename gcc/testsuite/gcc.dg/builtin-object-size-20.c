/* PR middle-end/92815 - spurious -Wstringop-overflow writing into
   a flexible array of an extern struct
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

#define ASSERT(expr) ((expr) ? (void)0 : fail (__LINE__))
#define bos0(expr) __builtin_object_size (expr, 1)
#define bos1(expr) __builtin_object_size (expr, 1)
#define bos2(expr) __builtin_object_size (expr, 2)
#define bos3(expr) __builtin_object_size (expr, 3)

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __SIZE_TYPE__  size_t;


extern void fail (int);


/* Verify sizes of a struct with a flexible array member and no padding.  */

struct ACX { char n, a[]; };

struct ACX ac0 = { };
struct ACX ac1 = { 1, { 1 } };
struct ACX ac2 = { 2, { 1, 2 } };
struct ACX ac3 = { 3, { 1, 2, 3 } };

extern struct ACX eacx;

void facx (void)
{
  ASSERT (bos0 (&ac0) == sizeof ac0);
  ASSERT (bos0 (&ac1) == 2);
  ASSERT (bos0 (&ac2) == 3);
  ASSERT (bos0 (&ac3) == 4);
  ASSERT (bos0 (&eacx) == (size_t)-1);

  ASSERT (bos1 (&ac0) == sizeof ac0);
  ASSERT (bos1 (&ac1) == 2);
  ASSERT (bos1 (&ac2) == 3);
  ASSERT (bos1 (&ac3) == 4);
  ASSERT (bos1 (&eacx) == (size_t)-1);

  ASSERT (bos2 (&ac0) == sizeof ac0);
  ASSERT (bos2 (&ac1) == 2);
  ASSERT (bos2 (&ac2) == 3);
  ASSERT (bos2 (&ac3) == 4);
  ASSERT (bos2 (&eacx) == sizeof eacx);

  ASSERT (bos3 (&ac0) == sizeof ac0);
  ASSERT (bos3 (&ac1) == 2);
  ASSERT (bos3 (&ac2) == 3);
  ASSERT (bos3 (&ac3) == 4);
  ASSERT (bos3 (&eacx) == sizeof eacx);
}



/* Verify sizes of a struct with a flexible array member and 1 byte
   of tail padding.  */

struct AI16CX { int16_t i; char n, a[]; };

struct AI16CX ai16c0 = { 0 };
struct AI16CX ai16c1 = { 0, 1, { 1 } };
struct AI16CX ai16c2 = { 0, 2, { 1, 2 } };
struct AI16CX ai16c3 = { 0, 3, { 1, 2, 3 } };
struct AI16CX ai16c4 = { 0, 4, { 1, 2, 3, 4 } };
struct AI16CX ai16c5 = { 0, 5, { 1, 2, 3, 4, 5 } };
struct AI16CX ai16c6 = { 0, 6, { 1, 2, 3, 4, 5, 6 } };
struct AI16CX ai16c7 = { 0, 7, { 1, 2, 3, 4, 5, 6, 7 } };
struct AI16CX ai16c8 = { 0, 8, { 1, 2, 3, 4, 5, 6, 7, 8 } };

extern struct AI16CX eai16cx;

void fai16cx (void)
{
  ASSERT (bos0 (&ai16c0) == sizeof ai16c0);
  ASSERT (bos0 (&ai16c1) == sizeof ai16c1);
  ASSERT (bos0 (&ai16c2) == sizeof ai16c2 + 1);
  ASSERT (bos0 (&ai16c3) == sizeof ai16c3 + 2);

  ASSERT (bos0 (&ai16c4) == sizeof ai16c4 + 3);
  ASSERT (bos0 (&ai16c5) == sizeof ai16c5 + 4);
  ASSERT (bos0 (&ai16c6) == sizeof ai16c6 + 5);
  ASSERT (bos0 (&ai16c7) == sizeof ai16c6 + 6);
  ASSERT (bos0 (&ai16c8) == sizeof ai16c6 + 7);

  ASSERT (bos0 (&eai16cx) == (size_t)-1);


  ASSERT (bos1 (&ai16c0) == sizeof ai16c0);
  ASSERT (bos1 (&ai16c1) == sizeof ai16c1);
  ASSERT (bos1 (&ai16c2) == sizeof ai16c2 + 1);
  ASSERT (bos1 (&ai16c3) == sizeof ai16c3 + 2);

  ASSERT (bos1 (&ai16c4) == sizeof ai16c4 + 3);
  ASSERT (bos1 (&ai16c5) == sizeof ai16c5 + 4);
  ASSERT (bos1 (&ai16c6) == sizeof ai16c6 + 5);
  ASSERT (bos1 (&ai16c7) == sizeof ai16c6 + 6);
  ASSERT (bos1 (&ai16c8) == sizeof ai16c6 + 7);

  ASSERT (bos1 (&eai16cx) == (size_t)-1);


  ASSERT (bos2 (&ai16c0) == sizeof ai16c0);
  ASSERT (bos2 (&ai16c1) == sizeof ai16c1);
  ASSERT (bos2 (&ai16c2) == sizeof ai16c2 + 1);
  ASSERT (bos2 (&ai16c3) == sizeof ai16c3 + 2);

  ASSERT (bos2 (&ai16c4) == sizeof ai16c4 + 3);
  ASSERT (bos2 (&ai16c5) == sizeof ai16c5 + 4);
  ASSERT (bos2 (&ai16c6) == sizeof ai16c6 + 5);
  ASSERT (bos2 (&ai16c7) == sizeof ai16c6 + 6);
  ASSERT (bos2 (&ai16c8) == sizeof ai16c6 + 7);

  ASSERT (bos2 (&eai16cx) == sizeof eai16cx);


  ASSERT (bos3 (&ai16c0) == sizeof ai16c0);
  ASSERT (bos3 (&ai16c1) == sizeof ai16c1);
  ASSERT (bos3 (&ai16c2) == sizeof ai16c2 + 1);
  ASSERT (bos3 (&ai16c3) == sizeof ai16c3 + 2);

  ASSERT (bos3 (&ai16c4) == sizeof ai16c4 + 3);
  ASSERT (bos3 (&ai16c5) == sizeof ai16c5 + 4);
  ASSERT (bos3 (&ai16c6) == sizeof ai16c6 + 5);
  ASSERT (bos3 (&ai16c7) == sizeof ai16c6 + 6);
  ASSERT (bos3 (&ai16c8) == sizeof ai16c6 + 7);

  ASSERT (bos3 (&eai16cx) == sizeof eai16cx);
}


/* Verify sizes of a struct with a flexible array member and 3 bytes
   of tail padding.  */

struct AI32CX { int32_t i; char n, a[]; };

struct AI32CX ai32c0 = { 0 };
struct AI32CX ai32c1 = { 0, 1, { 1 } };
struct AI32CX ai32c2 = { 0, 2, { 1, 2 } };
struct AI32CX ai32c3 = { 0, 3, { 1, 2, 3 } };
struct AI32CX ai32c4 = { 0, 4, { 1, 2, 3, 4 } };
struct AI32CX ai32c5 = { 0, 5, { 1, 2, 3, 4, 5 } };
struct AI32CX ai32c6 = { 0, 6, { 1, 2, 3, 4, 5, 6 } };
struct AI32CX ai32c7 = { 0, 7, { 1, 2, 3, 4, 5, 6, 7 } };
struct AI32CX ai32c8 = { 0, 8, { 1, 2, 3, 4, 5, 6, 7, 8 } };

extern struct AI32CX eai32cx;

void fai32cx (void)
{
  ASSERT (bos0 (&ai32c0) == sizeof ai32c0);
  ASSERT (bos0 (&ai32c1) == sizeof ai32c1);
  ASSERT (bos0 (&ai32c2) == sizeof ai32c2);
  ASSERT (bos0 (&ai32c3) == sizeof ai32c3);

  ASSERT (bos0 (&ai32c4) == sizeof ai32c4 + 1);
  ASSERT (bos0 (&ai32c5) == sizeof ai32c5 + 2);
  ASSERT (bos0 (&ai32c6) == sizeof ai32c6 + 3);
  ASSERT (bos0 (&ai32c7) == sizeof ai32c6 + 4);
  ASSERT (bos0 (&ai32c8) == sizeof ai32c6 + 5);

  ASSERT (bos0 (&eai32cx) == (size_t)-1);


  ASSERT (bos1 (&ai32c0) == sizeof ai32c0);
  ASSERT (bos1 (&ai32c1) == sizeof ai32c1);
  ASSERT (bos1 (&ai32c2) == sizeof ai32c2);
  ASSERT (bos1 (&ai32c3) == sizeof ai32c3);

  ASSERT (bos1 (&ai32c4) == sizeof ai32c4 + 1);
  ASSERT (bos1 (&ai32c5) == sizeof ai32c5 + 2);
  ASSERT (bos1 (&ai32c6) == sizeof ai32c6 + 3);
  ASSERT (bos1 (&ai32c7) == sizeof ai32c6 + 4);
  ASSERT (bos1 (&ai32c8) == sizeof ai32c6 + 5);

  ASSERT (bos1 (&eai32cx) == (size_t)-1);


  ASSERT (bos2 (&ai32c0) == sizeof ai32c0);
  ASSERT (bos2 (&ai32c1) == sizeof ai32c1);
  ASSERT (bos2 (&ai32c2) == sizeof ai32c2);
  ASSERT (bos2 (&ai32c3) == sizeof ai32c3);

  ASSERT (bos2 (&ai32c4) == sizeof ai32c4 + 1);
  ASSERT (bos2 (&ai32c5) == sizeof ai32c5 + 2);
  ASSERT (bos2 (&ai32c6) == sizeof ai32c6 + 3);
  ASSERT (bos2 (&ai32c7) == sizeof ai32c6 + 4);
  ASSERT (bos2 (&ai32c8) == sizeof ai32c6 + 5);

  ASSERT (bos2 (&eai32cx) == sizeof eai32cx);


  ASSERT (bos3 (&ai32c0) == sizeof ai32c0);
  ASSERT (bos3 (&ai32c1) == sizeof ai32c1);
  ASSERT (bos3 (&ai32c2) == sizeof ai32c2);
  ASSERT (bos3 (&ai32c3) == sizeof ai32c3);

  ASSERT (bos3 (&ai32c4) == sizeof ai32c4 + 1);
  ASSERT (bos3 (&ai32c5) == sizeof ai32c5 + 2);
  ASSERT (bos3 (&ai32c6) == sizeof ai32c6 + 3);
  ASSERT (bos3 (&ai32c7) == sizeof ai32c6 + 4);
  ASSERT (bos3 (&ai32c8) == sizeof ai32c6 + 5);

  ASSERT (bos3 (&eai32cx) == sizeof eai32cx);
}


/* Verify sizes of a struct with a flexible array member and 7 bytes
   of tail padding.  */

struct AI64CX { int64_t i __attribute__ ((aligned (8))); char n, a[]; };

struct AI64CX ai64c0 = { 0 };
struct AI64CX ai64c1 = { 0, 1, { 1 } };
struct AI64CX ai64c2 = { 0, 2, { 1, 2 } };
struct AI64CX ai64c3 = { 0, 3, { 1, 2, 3 } };
struct AI64CX ai64c4 = { 0, 4, { 1, 2, 3, 4 } };
struct AI64CX ai64c5 = { 0, 5, { 1, 2, 3, 4, 5 } };
struct AI64CX ai64c6 = { 0, 6, { 1, 2, 3, 4, 5, 6 } };
struct AI64CX ai64c7 = { 0, 7, { 1, 2, 3, 4, 5, 6, 7 } };
struct AI64CX ai64c8 = { 0, 8, { 1, 2, 3, 4, 5, 6, 7, 8 } };
struct AI64CX ai64c9 = { 0, 8, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } };

extern struct AI64CX eai64cx;

void fai64cx (void)
{
  ASSERT (bos0 (&ai64c0) == sizeof ai64c0);
  ASSERT (bos0 (&ai64c1) == sizeof ai64c1);
  ASSERT (bos0 (&ai64c2) == sizeof ai64c2);
  ASSERT (bos0 (&ai64c3) == sizeof ai64c3);
  ASSERT (bos0 (&ai64c4) == sizeof ai64c4);
  ASSERT (bos0 (&ai64c5) == sizeof ai64c5);
  ASSERT (bos0 (&ai64c6) == sizeof ai64c6);
  ASSERT (bos0 (&ai64c7) == sizeof ai64c7);

  ASSERT (bos0 (&ai64c8) == sizeof ai64c8 + 1);
  ASSERT (bos0 (&ai64c9) == sizeof ai64c9 + 2);

  ASSERT (bos0 (&eai64cx) == (size_t)-1);


  ASSERT (bos1 (&ai64c0) == sizeof ai64c0);
  ASSERT (bos1 (&ai64c1) == sizeof ai64c1);
  ASSERT (bos1 (&ai64c2) == sizeof ai64c2);
  ASSERT (bos1 (&ai64c3) == sizeof ai64c3);
  ASSERT (bos1 (&ai64c4) == sizeof ai64c4);
  ASSERT (bos1 (&ai64c5) == sizeof ai64c5);
  ASSERT (bos1 (&ai64c6) == sizeof ai64c6);
  ASSERT (bos1 (&ai64c7) == sizeof ai64c7);

  ASSERT (bos1 (&ai64c8) == sizeof ai64c8 + 1);
  ASSERT (bos1 (&ai64c9) == sizeof ai64c9 + 2);

  ASSERT (bos1 (&eai64cx) == (size_t)-1);


  ASSERT (bos2 (&ai64c0) == sizeof ai64c0);
  ASSERT (bos2 (&ai64c1) == sizeof ai64c1);
  ASSERT (bos2 (&ai64c2) == sizeof ai64c2);
  ASSERT (bos2 (&ai64c3) == sizeof ai64c3);
  ASSERT (bos2 (&ai64c4) == sizeof ai64c4);
  ASSERT (bos2 (&ai64c5) == sizeof ai64c5);
  ASSERT (bos2 (&ai64c6) == sizeof ai64c6);
  ASSERT (bos2 (&ai64c7) == sizeof ai64c7);

  ASSERT (bos2 (&ai64c8) == sizeof ai64c8 + 1);
  ASSERT (bos2 (&ai64c9) == sizeof ai64c9 + 2);

  ASSERT (bos2 (&eai64cx) == sizeof eai64cx);

  ASSERT (bos3 (&ai64c0) == sizeof ai64c0);
  ASSERT (bos3 (&ai64c1) == sizeof ai64c1);
  ASSERT (bos3 (&ai64c2) == sizeof ai64c2);
  ASSERT (bos3 (&ai64c3) == sizeof ai64c3);
  ASSERT (bos3 (&ai64c4) == sizeof ai64c4);
  ASSERT (bos3 (&ai64c5) == sizeof ai64c5);
  ASSERT (bos3 (&ai64c6) == sizeof ai64c6);
  ASSERT (bos3 (&ai64c7) == sizeof ai64c7);

  ASSERT (bos3 (&ai64c8) == sizeof ai64c8 + 1);
  ASSERT (bos3 (&ai64c9) == sizeof ai64c9 + 2);

  ASSERT (bos3 (&eai64cx) == sizeof eai64cx);
}


/* Also verify sizes of a struct with a zero length array member.  */

struct A0C0 { char n, a[0]; };

struct A0C0 a0c0 = { };
extern struct A0C0 ea0c0;

void fa0c0 (void)
{
  ASSERT (bos0 (&a0c0) == sizeof a0c0);
  ASSERT (bos0 (&ea0c0) == sizeof ea0c0);

  ASSERT (bos1 (&a0c0) == sizeof a0c0);
  ASSERT (bos1 (&a0c0) == sizeof ea0c0);

  ASSERT (bos2 (&a0c0) == sizeof a0c0);
  ASSERT (bos2 (&a0c0) == sizeof ea0c0);

  ASSERT (bos3 (&a0c0) == sizeof a0c0);
  ASSERT (bos3 (&a0c0) == sizeof ea0c0);
}

/* { dg-final { scan-tree-dump-not "fail" "optimized" } } */
