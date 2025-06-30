/* { dg-do compile } */
/* { dg-options "-O2 -Wnonnull" } */

#define N(x, y, z) __attribute__ ((nonnull_if_nonzero (x, y, z)))

void N (1, 2, 3) f1_1 (void *, int, int);

void N (1, 3, 4) f2_1 (void *, void *, int, int);
void N (1, 3, 4) N (2, 3, 4) f2_1_2 (void *, void *, int, int);

void N (1, 4, 6) N (3, 5, 7) f3_1_3 (void *, void *, void *, int, int, int, int);

void N (1, 5, 6) N (2, 5, 6) N (4, 5, 6) g4_1_2_4 (void *, void *, void *, void *, long, long);
void N (1, 5, 6) N (3, 5, 6) N (4, 5, 6) g4_1_3_4 (void *, void *, void *, void *, long, long);
void N (2, 5, 6) N (3, 5, 6) N (4, 5, 6) g4_2_3_4 (void *, void *, void *, void *, long, long);

void N (1, 17, 18) N (3, 17, 18) N (5, 17, 18) N (7, 17, 18) N (11, 17, 18) N (13, 17, 18)
g16_1_3_5_7_11_13 (void *, void *, void *, void *,
		   void *, void *, void *, void *,
		   void *, void *, void *, void *,
		   void *, void *, void *, void *, int, int);

static void *null (void) { return 0; }

void
test (int t, long u, int v, long w)
{
  void *p0 = null ();
  void *px = &px;

  f1_1 (p0, 0, 0);
  f1_1 (p0, 0, 4);
  f1_1 (p0, 3, 0);
  f1_1 (p0, t, v);
  f1_1 (p0, t, 0);
  f1_1 (p0, 0, v);
  f1_1 (p0, 42, 1); /* { dg-warning "argument 1 null where non-null expected because arguments 2 and 3 are nonzero" } */
  if (t && v)
    f1_1 (p0, t, v); /* { dg-warning "argument 1 null where non-null expected because arguments 2 and 3 are nonzero" } */
  f1_1 (px, 17, 17);

  f2_1 (p0, px, 0, 0);
  f2_1 (p0, px, 0, 3);
  f2_1 (p0, px, 7, 0);
  f2_1 (p0, px, t, v);
  f2_1 (p0, px, t, 0);
  f2_1 (p0, px, 0, v);
  f2_1 (p0, px, 5, 3);  /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  if (t > 4 && v > 8)
    f2_1 (p0, px, t, v);  /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  f2_1 (px, p0, 17, 17);
  f2_1 (p0, p0, 0, 0);
  f2_1 (p0, p0, 0, 4);
  f2_1 (p0, p0, 2, 0);
  if (t < 0 && v < -3)
    f2_1 (p0, p0, t, v);  /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */

  f2_1_2 (p0, p0, 0, 0);
  f2_1_2 (p0, p0, 0, 1);
  f2_1_2 (p0, p0, 2, 0);
  f2_1_2 (p0, p0, t, v);
  f2_1_2 (p0, p0, t, 0);
  f2_1_2 (p0, p0, 0, v);
  f2_1_2 (p0, px, 1, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  if (t > 8 && v >= 16)
    f2_1_2 (p0, px, t, v); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  f2_1_2 (px, p0, -3, -4); /* { dg-warning "argument 2 null where non-null expected because arguments 3 and 4 are nonzero" } */
  if (t < -2 && v >= 32)
    f2_1_2 (px, p0, t, v); /* { dg-warning "argument 2 null where non-null expected because arguments 3 and 4 are nonzero" } */
  f2_1_2 (p0, p0, 8, 165); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  /* { dg-warning "argument 2 null where non-null expected because arguments 3 and 4 are nonzero" "argument 2" { target *-*-* } .-1 } */
  if (t > 7 && v < -2)
    f2_1_2 (p0, p0, t, v); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  /* { dg-warning "argument 2 null where non-null expected because arguments 3 and 4 are nonzero" "argument 2" { target *-*-* } .-1 } */

  f3_1_3 (p0, p0, p0, 0, 0, 0, 0);
  f3_1_3 (p0, p0, p0, 0, 5, 4, 0);
  f3_1_3 (p0, p0, p0, 3, 0, 0, 6);
  f3_1_3 (p0, p0, px, 0, 6, 0, 6);
  f3_1_3 (p0, p0, px, 0, 6, 4, 6);
  f3_1_3 (p0, p0, px, 3, 6, 0, 6);
  f3_1_3 (px, p0, p0, 2, 0, 2, 0);
  f3_1_3 (px, p0, p0, 2, 0, 2, 4);
  f3_1_3 (px, p0, p0, 2, 6, 2, 0);
  f3_1_3 (p0, p0, p0, t, t, v, v);
  f3_1_3 (p0, p0, px, t, 6, v, 7);
  f3_1_3 (px, p0, p0, 2, t, 3, v);
  f3_1_3 (p0, px, px, 8, 2, 3, 5); /* { dg-warning "argument 1 null where non-null expected because arguments 4 and 6 are nonzero" } */
  if (t > 9 && v < -19)
    f3_1_3 (p0, px, px, t, 3, v, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 4 and 6 are nonzero" } */
  f3_1_3 (px, p0, px, 9, 10, 1, 2);
  if (t > 11 && v > 3)
    f3_1_3 (px, p0, px, t, t, v, v);
  f3_1_3 (px, px, p0, 10, 11, 2, 3); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 7 are nonzero" } */
  if (t < -5 && v > 2)
    f3_1_3 (px, px, p0, 0, t, 2, v); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 7 are nonzero" } */
  f3_1_3 (p0, p0, px, 11, 12, 1, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 4 and 6 are nonzero" } */
  if (t > 26 && v > 88)
    f3_1_3 (p0, p0, px, t, 3, v, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 4 and 6 are nonzero" } */
  f3_1_3 (px, p0, p0, 12, 13, 1, 2); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 7 are nonzero" } */
  if (t > 31 && v < -1)
    f3_1_3 (px, p0, p0, 12, t, 2, v); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 7 are nonzero" } */
  f3_1_3 (p0, p0, p0, 13, 14, 1, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 4 and 6 are nonzero" } */
  /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 7 are nonzero" "argument 3" { target *-*-* } .-1 } */
  if (t > 28 && v > 42)
    f3_1_3 (p0, p0, p0, t, t + 1, v, v + 1); /* { dg-warning "argument 1 null where non-null expected because arguments 4 and 6 are nonzero" } */
  /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 7 are nonzero" "argument 3" { target *-*-* } .-1 } */

  g4_1_2_4 (p0, px, px, px, u, w);
  g4_1_2_4 (px, p0, px, px, u, w);
  g4_1_2_4 (px, px, p0, px, u, w);
  g4_1_2_4 (px, px, px, p0, u, w);
  g4_1_2_4 (p0, px, px, px, 0, 0);
  g4_1_2_4 (p0, px, px, px, 0, 2);
  g4_1_2_4 (p0, px, px, px, 1, 0);
  g4_1_2_4 (px, p0, px, px, 0, 0);
  g4_1_2_4 (px, p0, px, px, 0, 3);
  g4_1_2_4 (px, p0, px, px, 4, 0);
  g4_1_2_4 (px, px, p0, px, 0, 0);
  g4_1_2_4 (px, px, p0, px, 0, 5);
  g4_1_2_4 (px, px, p0, px, 6, 0);
  g4_1_2_4 (px, px, px, p0, 0, 0);
  g4_1_2_4 (px, px, px, p0, 0, 7);
  g4_1_2_4 (px, px, px, p0, 8, 0);
  g4_1_2_4 (p0, px, px, px, 15, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u && w)
    g4_1_2_4 (p0, px, px, px, u, w); /* { dg-warning "argument 1 null where non-null expected because arguments 5 and 6 are nonzero" } */
  g4_1_2_4 (px, p0, px, px, 16, 2); /* { dg-warning "argument 2 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u > 2 && w > 3)
    g4_1_2_4 (px, p0, px, px, u, w); /* { dg-warning "argument 2 null where non-null expected because arguments 5 and 6 are nonzero" } */
  g4_1_2_4 (px, px, p0, px, 17, 8);
  if (u > 3 && w < -2)
    g4_1_2_4 (px, px, p0, px, u, w);
  g4_1_2_4 (px, px, px, p0, 18, 3); /* { dg-warning "argument 4 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if ((u < -2 || u > 10) && (w < -4 || w > 42))
    g4_1_2_4 (px, px, px, p0, u, w); /* { dg-warning "argument 4 null where non-null expected because arguments 5 and 6 are nonzero" } */

  g4_1_3_4 (p0, px, px, px, u, u);
  g4_1_3_4 (px, p0, px, px, u, u);
  g4_1_3_4 (px, px, p0, px, u, u);
  g4_1_3_4 (px, px, px, p0, u, u);
  g4_1_3_4 (p0, px, px, px, 0, 0);
  g4_1_3_4 (p0, px, px, px, 0, 1);
  g4_1_3_4 (p0, px, px, px, 2, 0);
  g4_1_3_4 (px, p0, px, px, 0, 0);
  g4_1_3_4 (px, p0, px, px, 0, 3);
  g4_1_3_4 (px, p0, px, px, 4, 0);
  g4_1_3_4 (px, px, p0, px, 0, 0);
  g4_1_3_4 (px, px, p0, px, 0, 5);
  g4_1_3_4 (px, px, p0, px, 6, 0);
  g4_1_3_4 (px, px, px, p0, 0, 0);
  g4_1_3_4 (px, px, px, p0, 0, 7);
  g4_1_3_4 (px, px, px, p0, 8, 0);
  g4_1_3_4 (p0, px, px, px, 20, 32); /* { dg-warning "argument 1 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u > 4 && w > 2)
    g4_1_3_4 (p0, px, px, px, u, w); /* { dg-warning "argument 1 null where non-null expected because arguments 5 and 6 are nonzero" } */
  g4_1_3_4 (px, p0, px, px, 21, 4);
  if ((u > 6 || u < -24) && (w > 8 || w < -5))
    g4_1_3_4 (px, p0, px, px, u, w);
  g4_1_3_4 (px, px, p0, px, 22, 4); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u > 9 && w > 13)
    g4_1_3_4 (px, px, p0, px, u - 3, w - 8); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 6 are nonzero" } */
  g4_1_3_4 (px, px, px, p0, 23, 8); /* { dg-warning "argument 4 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u > 10 && w > 12)
    g4_1_3_4 (px, px, px, p0, u, w); /* { dg-warning "argument 4 null where non-null expected because arguments 5 and 6 are nonzero" } */

  g4_2_3_4 (p0, px, px, px, u, u);
  g4_2_3_4 (px, p0, px, px, u, u);
  g4_2_3_4 (px, px, p0, px, u, u);
  g4_2_3_4 (px, px, px, p0, u, u);
  g4_2_3_4 (p0, px, px, px, 0, 0);
  g4_2_3_4 (p0, px, px, px, 0, 1);
  g4_2_3_4 (p0, px, px, px, 2, 0);
  g4_2_3_4 (px, p0, px, px, 0, 0);
  g4_2_3_4 (px, p0, px, px, 0, 3);
  g4_2_3_4 (px, p0, px, px, 4, 0);
  g4_2_3_4 (px, px, p0, px, 0, 0);
  g4_2_3_4 (px, px, p0, px, 0, 5);
  g4_2_3_4 (px, px, p0, px, 6, 0);
  g4_2_3_4 (px, px, px, p0, 0, 0);
  g4_2_3_4 (px, px, px, p0, 0, 7);
  g4_2_3_4 (px, px, px, p0, 8, 0);
  g4_2_3_4 (p0, px, px, px, 1, 2);
  if (u > 12 && w > 16)
    g4_2_3_4 (p0, px, px, px, u, w);
  g4_2_3_4 (px, p0, px, px, 2, 3); /* { dg-warning "argument 2 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u > 17 && w > 19)
    g4_2_3_4 (px, p0, px, px, u - 3, w - 2); /* { dg-warning "argument 2 null where non-null expected because arguments 5 and 6 are nonzero" } */
  g4_2_3_4 (px, px, p0, px, 3, 8); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u > 24 && w > 22)
    g4_2_3_4 (px, px, p0, px, u, w); /* { dg-warning "argument 3 null where non-null expected because arguments 5 and 6 are nonzero" } */
  g4_2_3_4 (px, px, px, p0, 4, 2); /* { dg-warning "argument 4 null where non-null expected because arguments 5 and 6 are nonzero" } */
  if (u > 42 && w > 48)
    g4_2_3_4 (px, px, px, p0, u, w); /* { dg-warning "argument 4 null where non-null expected because arguments 5 and 6 are nonzero" } */

  g16_1_3_5_7_11_13 (px, px, px, px, px, px, px, px,
		     px, px, px, px, px, px, px, px, 17, 18);
  g16_1_3_5_7_11_13 (p0, p0, p0, p0, p0, p0, p0, p0,
		     p0, p0, p0, p0, p0, p0, p0, p0, t, v);
  g16_1_3_5_7_11_13 (p0, p0, p0, p0, p0, p0, p0, p0,
		     p0, p0, p0, p0, p0, p0, p0, p0, 0, 0);
  g16_1_3_5_7_11_13 (p0, p0, p0, p0, p0, p0, p0, p0,
		     p0, p0, p0, p0, p0, p0, p0, p0, 0, 4);
  g16_1_3_5_7_11_13 (p0, p0, p0, p0, p0, p0, p0, p0,
		     p0, p0, p0, p0, p0, p0, p0, p0, 3, 0);

  g16_1_3_5_7_11_13 (px, p0, px, p0, px, p0, px, p0, p0, p0, px, p0, p0, p0, p0, p0, 2, 1); /* { dg-warning "argument 13 null where non-null expected because arguments 17 and 18 are nonzero" } */
  if (t > 122 && v > 18)
    g16_1_3_5_7_11_13 (px, p0, px, p0, px, p0, px, p0, p0, p0, px, p0, p0, p0, p0, p0, t, v); /* { dg-warning "argument 13 null where non-null expected because arguments 17 and 18 are nonzero" } */
}
