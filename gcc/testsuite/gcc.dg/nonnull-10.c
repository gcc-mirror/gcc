/* { dg-do compile } */
/* { dg-options "-O2 -Wnonnull" } */

#define N(x, y) __attribute__ ((nonnull_if_nonzero (x, y)))

void N (1, 2) f1_1 (void *, int);

void N (1, 3) f2_1 (void *, void *, int);
void N (1, 3) N (2, 3) f2_1_2 (void *, void *, int);

void N (1, 4) N (3, 5) f3_1_3 (void *, void *, void *, int, int);

void N (1, 5) N (2, 5) N (4, 5) g4_1_2_4 (void *, void *, void *, void *, long);
void N (1, 5) N (3, 5) N (4, 5) g4_1_3_4 (void *, void *, void *, void *, long);
void N (2, 5) N (3, 5) N (4, 5) g4_2_3_4 (void *, void *, void *, void *, long);

void N (1, 17) N (3, 17) N (5, 17) N (7, 17) N (11, 17) N (13, 17)
g16_1_3_5_7_11_13 (void *, void *, void *, void *,
		   void *, void *, void *, void *,
		   void *, void *, void *, void *,
		   void *, void *, void *, void *, int);

static void *null (void) { return 0; }

void
test (int t, long u)
{
  void *p0 = null ();
  void *px = &px;

  f1_1 (p0, 0);
  f1_1 (p0, t);
  f1_1 (p0, 42); /* { dg-warning "argument 1 null where non-null expected because argument 2 is nonzero" } */
  if (t)
    f1_1 (p0, t); /* { dg-warning "argument 1 null where non-null expected because argument 2 is nonzero" } */
  f1_1 (px, 17);

  f2_1 (p0, px, 0);
  f2_1 (p0, px, t);
  f2_1 (p0, px, 5);  /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  if (t > 4)
    f2_1 (p0, px, t);  /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  f2_1 (px, p0, 17);
  f2_1 (p0, p0, 0);
  if (t < 0)
    f2_1 (p0, p0, t);  /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */

  f2_1_2 (p0, p0, 0);
  f2_1_2 (p0, p0, t);
  f2_1_2 (p0, px, 1); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  if (t > 8)
    f2_1_2 (p0, px, t); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  f2_1_2 (px, p0, -3); /* { dg-warning "argument 2 null where non-null expected because argument 3 is nonzero" } */
  if (t < -2)
    f2_1_2 (px, p0, t); /* { dg-warning "argument 2 null where non-null expected because argument 3 is nonzero" } */
  f2_1_2 (p0, p0, 8); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  /* { dg-warning "argument 2 null where non-null expected because argument 3 is nonzero" "argument 2" { target *-*-* } .-1 } */
  if (t > 7)
    f2_1_2 (p0, p0, t); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  /* { dg-warning "argument 2 null where non-null expected because argument 3 is nonzero" "argument 2" { target *-*-* } .-1 } */

  f3_1_3 (p0, p0, p0, 0, 0);
  f3_1_3 (p0, p0, px, 0, 6);
  f3_1_3 (px, p0, p0, 2, 0);
  f3_1_3 (p0, p0, p0, t, t);
  f3_1_3 (p0, p0, px, t, 6);
  f3_1_3 (px, p0, p0, 2, t);
  f3_1_3 (p0, px, px, 8, 2); /* { dg-warning "argument 1 null where non-null expected because argument 4 is nonzero" } */
  if (t > 9)
    f3_1_3 (p0, px, px, t, 3); /* { dg-warning "argument 1 null where non-null expected because argument 4 is nonzero" } */
  f3_1_3 (px, p0, px, 9, 10);
  if (t > 11)
    f3_1_3 (px, p0, px, t, t);
  f3_1_3 (px, px, p0, 10, 11); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  if (t < -5)
    f3_1_3 (px, px, p0, 0, t); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  f3_1_3 (p0, p0, px, 11, 12); /* { dg-warning "argument 1 null where non-null expected because argument 4 is nonzero" } */
  if (t > 26)
    f3_1_3 (p0, p0, px, t, 0); /* { dg-warning "argument 1 null where non-null expected because argument 4 is nonzero" } */
  f3_1_3 (px, p0, p0, 12, 13); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  if (t > 31)
    f3_1_3 (px, p0, p0, 12, t); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  f3_1_3 (p0, p0, p0, 13, 14); /* { dg-warning "argument 1 null where non-null expected because argument 4 is nonzero" } */
  /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" "argument 3" { target *-*-* } .-1 } */
  if (t > 28)
    f3_1_3 (p0, p0, p0, t, t + 1); /* { dg-warning "argument 1 null where non-null expected because argument 4 is nonzero" } */
  /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" "argument 3" { target *-*-* } .-1 } */

  g4_1_2_4 (p0, px, px, px, u);
  g4_1_2_4 (px, p0, px, px, u);
  g4_1_2_4 (px, px, p0, px, u);
  g4_1_2_4 (px, px, px, p0, u);
  g4_1_2_4 (p0, px, px, px, 0);
  g4_1_2_4 (px, p0, px, px, 0);
  g4_1_2_4 (px, px, p0, px, 0);
  g4_1_2_4 (px, px, px, p0, 0);
  g4_1_2_4 (p0, px, px, px, 15); /* { dg-warning "argument 1 null where non-null expected because argument 5 is nonzero" } */
  if (u)
    g4_1_2_4 (p0, px, px, px, u); /* { dg-warning "argument 1 null where non-null expected because argument 5 is nonzero" } */
  g4_1_2_4 (px, p0, px, px, 16); /* { dg-warning "argument 2 null where non-null expected because argument 5 is nonzero" } */
  if (u > 2)
    g4_1_2_4 (px, p0, px, px, u); /* { dg-warning "argument 2 null where non-null expected because argument 5 is nonzero" } */
  g4_1_2_4 (px, px, p0, px, 17);
  if (u > 3)
    g4_1_2_4 (px, px, p0, px, u);
  g4_1_2_4 (px, px, px, p0, 18); /* { dg-warning "argument 4 null where non-null expected because argument 5 is nonzero" } */
  if (u < -2 || u > 10)
    g4_1_2_4 (px, px, px, p0, u); /* { dg-warning "argument 4 null where non-null expected because argument 5 is nonzero" } */

  g4_1_3_4 (p0, px, px, px, u);
  g4_1_3_4 (px, p0, px, px, u);
  g4_1_3_4 (px, px, p0, px, u);
  g4_1_3_4 (px, px, px, p0, u);
  g4_1_3_4 (p0, px, px, px, 0);
  g4_1_3_4 (px, p0, px, px, 0);
  g4_1_3_4 (px, px, p0, px, 0);
  g4_1_3_4 (px, px, px, p0, 0);
  g4_1_3_4 (p0, px, px, px, 20); /* { dg-warning "argument 1 null where non-null expected because argument 5 is nonzero" } */
  if (u > 4)
    g4_1_3_4 (p0, px, px, px, u); /* { dg-warning "argument 1 null where non-null expected because argument 5 is nonzero" } */
  g4_1_3_4 (px, p0, px, px, 21);
  if (u > 6 || u < -24)
    g4_1_3_4 (px, p0, px, px, u);
  g4_1_3_4 (px, px, p0, px, 22); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  if (u > 9)
    g4_1_3_4 (px, px, p0, px, u - 3); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  g4_1_3_4 (px, px, px, p0, 23); /* { dg-warning "argument 4 null where non-null expected because argument 5 is nonzero" } */
  if (u > 10)
    g4_1_3_4 (px, px, px, p0, u); /* { dg-warning "argument 4 null where non-null expected because argument 5 is nonzero" } */

  g4_2_3_4 (p0, px, px, px, u);
  g4_2_3_4 (px, p0, px, px, u);
  g4_2_3_4 (px, px, p0, px, u);
  g4_2_3_4 (px, px, px, p0, u);
  g4_2_3_4 (p0, px, px, px, 0);
  g4_2_3_4 (px, p0, px, px, 0);
  g4_2_3_4 (px, px, p0, px, 0);
  g4_2_3_4 (px, px, px, p0, 0);
  g4_2_3_4 (p0, px, px, px, 1);
  if (u > 12)
    g4_2_3_4 (p0, px, px, px, u);
  g4_2_3_4 (px, p0, px, px, 2); /* { dg-warning "argument 2 null where non-null expected because argument 5 is nonzero" } */
  if (u > 17)
    g4_2_3_4 (px, p0, px, px, u - 3); /* { dg-warning "argument 2 null where non-null expected because argument 5 is nonzero" } */
  g4_2_3_4 (px, px, p0, px, 3); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  if (u > 24)
    g4_2_3_4 (px, px, p0, px, u); /* { dg-warning "argument 3 null where non-null expected because argument 5 is nonzero" } */
  g4_2_3_4 (px, px, px, p0, 4); /* { dg-warning "argument 4 null where non-null expected because argument 5 is nonzero" } */
  if (u > 42)
    g4_2_3_4 (px, px, px, p0, u); /* { dg-warning "argument 4 null where non-null expected because argument 5 is nonzero" } */

  g16_1_3_5_7_11_13 (px, px, px, px, px, px, px, px,
		     px, px, px, px, px, px, px, px, 17);
  g16_1_3_5_7_11_13 (p0, p0, p0, p0, p0, p0, p0, p0,
		     p0, p0, p0, p0, p0, p0, p0, p0, t);
  g16_1_3_5_7_11_13 (p0, p0, p0, p0, p0, p0, p0, p0,
		     p0, p0, p0, p0, p0, p0, p0, p0, 0);

  g16_1_3_5_7_11_13 (px, p0, px, p0, px, p0, px, p0, p0, p0, px, p0, p0, p0, p0, p0, 2); /* { dg-warning "argument 13 null where non-null expected because argument 17 is nonzero" } */
  if (t > 122)
    g16_1_3_5_7_11_13 (px, p0, px, p0, px, p0, px, p0, p0, p0, px, p0, p0, p0, p0, p0, t); /* { dg-warning "argument 13 null where non-null expected because argument 17 is nonzero" } */
}
