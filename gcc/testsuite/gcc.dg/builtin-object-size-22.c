/* PR middle-end/92815 - a variant of gcc.dg/builtin-object-size-20.c
   prepared for all targets, irregardless if they pack or not
   the structs by default.
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

#define ASSERT(expr) ((expr) ? (void)0 : fail (__LINE__))
#define bos0(expr) __builtin_object_size (expr, 1)
#define bos1(expr) __builtin_object_size (expr, 1)
#define bos2(expr) __builtin_object_size (expr, 2)
#define bos3(expr) __builtin_object_size (expr, 3)

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
