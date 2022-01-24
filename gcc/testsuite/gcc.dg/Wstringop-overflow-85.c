/* PR middle-end/103215 - bogus -Wstringop-overflow with two pointers with
   different offsets each
   Test for accesses into distinct arrays through pointers with different
   offsets each.

   If/when -Wstringop-overflow is enhanced to issue "maybe" kinds of
   warnings some of the accesses here will trigger those and will need
   updating.

   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#define NOIPA __attribute__ ((noipa))

void sink (int[1]);
#define A(p, off) sink (p + off)

extern int a4[4], a8[8];




NOIPA void a4_p1_a8_p3 (int i)
{
  int *a4_p1 = a4 + 1;
  int *a8_p3 = a8 + 3;
  int *q = i ? a4_p1 : a8_p3;
  A (q, -4);      // { dg-warning "-Wstringop-overflow" }
  /* Because -3 is a valid offset into a8 but not a4, q must point
     to the former and so subscripts between -3 and +4 refer to its
     elements.  */
  A (q, -3); A (q, -2); A (q, -1); A (q, 0);
  A (q,  1); A (q,  2); A (q,  3); A (q, 4);
  A (q, 5);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  /* Both of the following are definitely out of bounds but the first isn't
     diagnosed because the code conservatively merges the offsets into A4
     and A8.  */
  A (q, 7);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void a4_p1_a8_p5 (int i)
{
  int *a4_p1 = a4 + 1;
  int *a8_p5 = a8 + 5;
  int *q = i ? a4_p1 : a8_p5;
  A (q, -6);     // { dg-warning "-Wstringop-overflow" }
  /* Similarly to the above, because -5 is a valid offset into a8 but
     not a4, q must point to the former and so subscripts between -5
     and +2 refer to its elements.  */
  A (q, -5); A (q, -4); A (q, -3); A (q, -2);
  A (q, -1); A (q,  0); A (q,  1); A (q,  2);
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 7);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void a4_p1_a8_p7 (int i)
{
  int *a4_p1 = a4 + 1;
  int *a8_p7 = a8 + 7;
  int *q = i ? a4_p1 : a8_p7;
  A (q, -8);     // { dg-warning "-Wstringop-overflow" }
  A (q, -7); A (q, -6); A (q, -5); A (q, -4);
  A (q, -3); A (q, -2); A (q, -1); A (q,  0);
  /* Since -7 is valid, q must point to a8 and the last valid subscript
     must be 0.  */
  A (q, 1);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 7);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void mp_1_a4_p1_a8_p7 (int i, int j)
{
  int *a4_p1 = a4 + 1;
  int *a8_p7 = a8 + 7;
  int *p = i ? a4_p1 : a8_p7;
  int *q = j ? p + 1 : p - 1;

  A (q, -9);      // { dg-warning "-Wstringop-overflow" }

  /* q points either to a8 + [6, 8] or a4 + [0, 2].  */
  A (q, -8); A (q, -7); A (q, -6); A (q, -5);
  A (q, -4); A (q, -3); A (q, -2); A (q, -1);

  /* Since all the above are valid, q must point to a8 + 8. But as
     mentioned above, the warning for each subscript is independent
     of prior subscripts into the same object so the access below
     aren't diagnosed.  */
  A (q, 0);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 1);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 2);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 4);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 8);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void mp1_a4_p1_a8_p5 (int i, int j)
{
  int *a4_p1 = a4 + 1;
  int *a8_p5 = a8 + 5;
  int *p = i ? a4_p1 : a8_p5;

  int *q = j ? p + 1 : p - 1;

  // q is assumed to point to a8 + 6
  A (q, -7);      // { dg-warning "-Wstringop-overflow" }
  A (q, -6); A (q, -5); A (q, -4); A (q, -3);
  A (q, -2); A (q, -1); A (q,  0); A (q,  1);
  /* Even though the above accesses rule it out, q is now assumed
     to point to either a4 + [0, 2] or a8 + [4, 5].  */
  A (q, 2);
  /* q is now assumed to point tp a4.  Given that, only the first store
     is valid.  */
  A (q, 3);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 4);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 5);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 6);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 7);       // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q, 8);       // { dg-warning "-Wstringop-overflow" }
}


NOIPA void mp1_a4_p1_a8_p4 (int i, int j)
{
  int *a4_p1 = a4 + 1;
  int *a8_p4 = a8 + 4;
  int *p = i ? a4_p1 : a8_p4;

  int *q = j ? p + 1 : p - 1;

  // q is assumed to point to a8 + 5
  A (q, -6);      // { dg-warning "-Wstringop-overflow" }
  A (q, -5);
  A (q, -4);
  A (q, -3);
  A (q, -2);
  A (q, -1);
  A (q,  0);
  A (q,  1);
  A (q,  2);
  /* Even though the above accesses rule it out, q is now assumed
     to point tp a4.  Given that, only the first store is valid.  */
  A (q,  3);      // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q,  4);      // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q,  5);      // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q,  6);      // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q,  7);      // { dg-warning "-Wstringop-overflow" "pr??????" { xfail *-*-* } }
  A (q,  8);      // { dg-warning "-Wstringop-overflow" }
}
