/* PR tree-optimization/78450 - strlen(s) return value can be assumed
   to be less than the size of s
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

#include "strlenopt.h"

extern char ax[];

struct MemArray7 { char a7[7]; };
struct MemArray6 { char a6[6]; };
struct MemArray5 { char a5[5]; };
struct MemArray4 { char a4[4]; };
struct MemArray3 { char a3[3]; };
struct MemArray2 { char a2[2]; };
struct MemArray1 { char a1[1]; };
struct MemArray0 { int n; char a0[0]; };
struct MemArrayX { int n; char ax[]; };

struct MemArrays
{
  struct MemArray7 *ma7;
  struct MemArray6 *ma6;
  struct MemArray5 *ma5;
  struct MemArray4 *ma4;
  struct MemArray3 *ma3;
  struct MemArray2 *ma2;
  struct MemArray1 *ma1;
  struct MemArray0 *ma0;
  struct MemArrayX *max;
};

extern void if_stmt_on_line (int);
extern void else_stmt_on_line (int);

#define T(expr)								\
  (!!(expr) ? if_stmt_on_line (__LINE__) : else_stmt_on_line (__LINE__))

void test_memarray_lt (struct MemArrays *p)
{
  T (strlen (p->ma7->a7) < sizeof p->ma7->a7);
  T (strlen (p->ma6->a6) < sizeof p->ma6->a6);
  T (strlen (p->ma5->a5) < sizeof p->ma5->a5);
  T (strlen (p->ma4->a4) < sizeof p->ma4->a4);
  T (strlen (p->ma3->a3) < sizeof p->ma3->a3);
  T (strlen (p->ma2->a2) < sizeof p->ma2->a2);
  T (strlen (p->ma1->a1) < sizeof p->ma1->a1);

  T (strlen (p->ma0->a0) < 1);
  T (strlen (p->max->ax) < 1);
}

void test_memarray_eq (struct MemArrays *p)
{
  T (strlen (p->ma7->a7) == sizeof p->ma7->a7);
  T (strlen (p->ma6->a6) == sizeof p->ma6->a6);
  T (strlen (p->ma5->a5) == sizeof p->ma5->a5);
  T (strlen (p->ma4->a4) == sizeof p->ma4->a4);
  T (strlen (p->ma3->a3) == sizeof p->ma3->a3);
  T (strlen (p->ma2->a2) == sizeof p->ma2->a2);
  T (strlen (p->ma1->a1) == sizeof p->ma1->a1);

  T (strlen (p->ma0->a0) == 1);
  T (strlen (p->max->ax) == 1);
}

void test_memarray_gt (struct MemArrays *p)
{
  T (strlen (p->ma7->a7) > sizeof p->ma7->a7);
  T (strlen (p->ma6->a6) > sizeof p->ma6->a6);
  T (strlen (p->ma5->a5) > sizeof p->ma5->a5);
  T (strlen (p->ma4->a4) > sizeof p->ma4->a4);
  T (strlen (p->ma3->a3) > sizeof p->ma3->a3);
  T (strlen (p->ma2->a2) > sizeof p->ma2->a2);
  T (strlen (p->ma1->a1) > sizeof p->ma1->a1);

  T (strlen (p->ma0->a0) > 1);
  T (strlen (p->max->ax) > 1);
 }

/* Verify that no if or else statements have been eliminated.
   { dg-final { scan-tree-dump-times "if_stmt_on_line" 27 "optimized" } }
   { dg-final { scan-tree-dump-times "else_stmt_on_line" 27 "optimized" } }  */
