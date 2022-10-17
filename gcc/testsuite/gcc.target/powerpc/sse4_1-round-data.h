/* Pick a few numbers at random which are not in the input data and
   unlikely to show up naturally.  */
#define PASSTHROUGH -29.5
#define IGNORED -61.5

union value {
  VEC_T x;
  FP_T f[sizeof (VEC_T) / sizeof (FP_T)];
};

struct data {
  union value value;
  double answer[sizeof (VEC_T) / sizeof (FP_T)];
};

struct data2 {
  union value value1;
  union value value2;
  double answer[sizeof (VEC_T) / sizeof (FP_T)];
};
