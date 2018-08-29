/* PR c/78673 - sprintf missing attribute nonnull on destination argument
   Test to verify that calls to user-defined functions declared with
   the "nonnull" function attribute are diagnosed.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wnonnull" } */

#define N(...) __attribute__ ((nonnull (__VA_ARGS__)))

void N (1) f1_1 (void*);

void N (1)       f2_1 (void*, void*);
void N (1) N (2) f2_1_2 (void*, void*);

void N (1) N (3) f3_1_3 (void*, void*, void*);

void N (1, 2) N (4) g4_1_2_4 (void*, void*, void*, void*);
void N (1, 3) N (4) g4_1_3_4 (void*, void*, void*, void*);
void N (2, 3, 4)    g4_2_3_4 (void*, void*, void*, void*);

void N () g4_all (void*, void*, void*, void*);

void N (1, 3, 5, 7, 11, 13)
g16_1_3_5_7_11_13 (void*, void*, void*, void*,
		   void*, void*, void*, void*,
		   void*, void*, void*, void*,
		   void*, void*, void*, void*);

static void* null (void) { return 0; }

void test (void)
{
  void *p0 = null ();
  void *px = &px;

  f1_1 (p0);   /* { dg-warning "argument 1 null where non-null expected " } */
  f1_1 (px);

  f2_1 (p0, px);  /* { dg-warning "argument 1 null" } */
  f2_1 (px, p0);
  f2_1 (p0, p0);  /* { dg-warning "argument 1 null" } */

  f2_1_2 (p0, px);  /* { dg-warning "argument 1 null" } */
  f2_1_2 (px, p0);  /* { dg-warning "argument 2 null" } */
  f2_1_2 (p0, p0);  /* { dg-warning "argument 1 null" } */
  /* { dg-warning "argument 2 null" "argument 2" { target *-*-* } .-1 } */

  f3_1_3 (p0, px, px);  /* { dg-warning "argument 1 null" } */
  f3_1_3 (px, p0, px);
  f3_1_3 (px, px, p0);  /* { dg-warning "argument 3 null" } */
  f3_1_3 (p0, p0, px);  /* { dg-warning "argument 1 null" } */
  f3_1_3 (px, p0, p0);  /* { dg-warning "argument 3 null" } */
  f3_1_3 (p0, p0, p0);  /* { dg-warning "argument 1 null" } */
  /* { dg-warning "argument 3 null" "argument 3" { target *-*-* } .-1 } */

  g4_1_2_4 (p0, px, px, px);  /* { dg-warning "argument 1 null" } */
  g4_1_2_4 (px, p0, px, px);  /* { dg-warning "argument 2 null" } */
  g4_1_2_4 (px, px, p0, px);
  g4_1_2_4 (px, px, px, p0);  /* { dg-warning "argument 4 null" } */

  g4_1_3_4 (p0, px, px, px);  /* { dg-warning "argument 1 null" } */
  g4_1_3_4 (px, p0, px, px);
  g4_1_3_4 (px, px, p0, px);  /* { dg-warning "argument 3 null" } */
  g4_1_3_4 (px, px, px, p0);  /* { dg-warning "argument 4 null" } */

  g4_2_3_4 (p0, px, px, px);
  g4_2_3_4 (px, p0, px, px);  /* { dg-warning "argument 2 null" } */
  g4_2_3_4 (px, px, p0, px);  /* { dg-warning "argument 3 null" } */
  g4_2_3_4 (px, px, px, p0);  /* { dg-warning "argument 4 null" } */

  g4_all (p0, px, px, px);  /* { dg-warning "argument 1 null" } */
  g4_all (px, p0, px, px);  /* { dg-warning "argument 2 null" } */
  g4_all (px, px, p0, px);  /* { dg-warning "argument 3 null" } */
  g4_all (px, px, px, p0);  /* { dg-warning "argument 4 null" } */

  g16_1_3_5_7_11_13 (px, px, px, px, px, px, px, px,
		     px, px, px, px, px, px, px, px);

  g16_1_3_5_7_11_13 (px, p0, px, p0, px, p0, px, p0, p0, p0, px, p0, p0, p0, p0, p0);   /* { dg-warning "argument 13 null" } */
}
