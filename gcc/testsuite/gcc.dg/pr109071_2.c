/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to code duplication from jump threading.
   test case is from PR85788, which is a duplication of PR109071.  */  
/* { dg-options "-O2 -Warray-bounds -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
int b=10;
int *d = &b, *e;
void a(void *k, long l) {
  long f = __builtin_object_size(k, 0);
  __builtin___memset_chk(k, b, l, f); /* { dg-warning "is out of the bounds" } */
}
typedef struct {
  int g;
  int h;
  char i[8000 * 8];
} j;
static void make_str_raster(j *k) {
  int *c = d;
  for (; c; c = e)
    k->g = k->h = 32767;

  a(k->i, k->g / 8 * k->h);
  for (; d;)
    ;
}
j m;
void n() { make_str_raster(&m); }
/* { dg-begin-multiline-output "" }
   NN |   __builtin___memset_chk(k, b, l, f);
      |   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  'n': events 1-2
   NN |   __builtin___memset_chk(k, b, l, f);
      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      |   |
      |   (2) warning happens here
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |   for (; c; c = e)
      |          ^
      |          |
      |          (1) when the condition is evaluated to false
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | j m;
      |   ^
   { dg-end-multiline-output "" } */
