/* PR middle-end/103215 - bogus -Warray-bounds with two pointers with
   different offsets each
   Test for accesses by a user-defined function into the same array
   through pointers with different offsets each.  See Warray-bounds-91.c
   for the corresponding test exercising -Warray-bounds for direct accesses.
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#define NOIPA __attribute__ ((noipa))

void sink (int[1]);
#define A(p, off) sink (p + off)

extern int a4[4];


NOIPA void p0_p1 (int i)
{
  int *p0 = a4 + 0;
  int *p1 = a4 + 1;
  int *q = i ? p0 : p1;
  A (q, -2);      // { dg-warning "-Wstringop-overflow" }
  A (q, -1); A (q, 0); A (q, 1); A (q, 2);
  /* Since q points to a4 and -1 is a valid subscript, +3 must be invalid.
     But the warning for each subscript is independent of prior subscripts
     into the same object.  That should be improved.  */
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 4);       // { dg-warning "-Wstringop-overflow" }
}

NOIPA void p1_p0 (int i)
{
  int *p1 = a4 + 1;
  int *p0 = a4 + 0;
  int *q = i ? p0 : p1;
  A (q, -2);      // { dg-warning "-Wstringop-overflow" }
  A (q, -1); A (q, 0); A (q, 1); A (q, 2);
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 4);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void p1_p2 (int i)
{
  int *p1 = a4 + 1;
  int *p2 = a4 + 2;
  int *q = i ? p1 : p2;
  A (q, -3);      // { dg-warning "-Wstringop-overflow" }
  A (q, -2); A (q, -1); A (q, 0); A (q, 1);
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" }
}

NOIPA void p2_p1 (int i)
{
  int *p2 = a4 + 2;
  int *p1 = a4 + 1;
  int *q = i ? p1 : p2;
  A (q, -3);      // { dg-warning "-Wstringop-overflow" }
  A (q, -2); A (q, -1); A (q, 0); A (q, 1);
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void p1_p3 (int i)
{
  int *p1 = a4 + 1;
  int *p3 = a4 + 3;
  int *q = i ? p1 : p3;
  A (q, -4);      // { dg-warning "-Wstringop-overflow" }
  A (q, -3); A (q, -2); A (q, -1); A (q, 0);
  A (q, 1);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" }
}

NOIPA void p3_p1 (int i)
{
  int *p3 = a4 + 3;
  int *p1 = a4 + 1;
  int *q = i ? p1 : p3;
  A (q, -4);      // { dg-warning "-Wstringop-overflow" }
  A (q, -3); A (q, -2); A (q, -1); A (q, 0);
  A (q, 1);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void p1_p4 (int i)
{
  int *p1 = a4 + 1;
  int *p4 = a4 + 4;
  int *q = i ? p1 : p4;
  A (q, -5);      // { dg-warning "-Wstringop-overflow" }
  A (q, -4); A (q, -3); A (q, -2); A (q, -1);
  A (q, 0);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 1);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" }
}

NOIPA void p4_p1 (int i)
{
  int *p4 = a4 + 4;
  int *p1 = a4 + 1;
  int *q = i ? p1 : p4;
  A (q, -5);      // { dg-warning "-Wstringop-overflow" }
  A (q, -4); A (q, -3); A (q, -2); A (q, -1);
  A (q, 0);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 1);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void p0_p1_p2 (int i)
{
  int *p0 = a4 + 0;
  int *p1 = a4 + 1;
  int *p2 = a4 + 2;
  int *q = i < 0 ? p1 : 0 < i ? p2 : p0;
  A (q, -3);      // { dg-warning "-Wstringop-overflow" }
  A (q, -2); A (q, -1); A (q, 0); A (q, 1);
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 4);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void p0_p1_p2_p3_p4 (int i)
{
  int *p0 = a4 + 0;
  int *p1 = a4 + 1;
  int *p2 = a4 + 2;
  int *p3 = a4 + 3;
  int *p4 = a4 + 4;
  int *q = i < -1 ? p1 : i < 0 ? p2 : 1 < i ? p4 : 0 < i ? p3 : p0;
  A (q, -5);      // { dg-warning "-Wstringop-overflow" }
  A (q, -4); A (q, -3); A (q, -2); A (q, -1);
  A (q, 0);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 1);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 4);       // { dg-warning "-Wstringop-overflow" }
}
