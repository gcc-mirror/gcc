/* { dg-do run } */
/* { dg-require-effective-target ultrasparc_hw } */
/* { dg-options "-mcpu=ultrasparc -mvis -O1" } */

typedef long long int64_t;
typedef unsigned char vec8 __attribute__((vector_size(8)));

extern void abort ();
extern void exit (int);

#define _(A) (unsigned char)A

int64_t foo (vec8 a, vec8 b) {
  int64_t d = 2;
  d = __builtin_vis_pdist (a, b, d);
  return d;
}

int64_t bar () {
  int64_t d = 2;
  vec8 a = { _(1), _(2), _(3), _(4), _(5), _(6), _(7), _(255) };
  vec8 b = { _(2), _(4), _(8), _(16), _(32), _(64), _(128), _(8) };
  d = __builtin_vis_pdist (a, b, d);
  return d;
}


static vec8 a = { 1, 2, 3, 4, 5, 6, 7, 255 };
static vec8 b = { 2, 4, 8, 16, 32, 64, 128, 8 };

int main (int argc, char *argv[]) {

  if (foo (a, b) != bar ())
    abort ();

  exit (0);
}
