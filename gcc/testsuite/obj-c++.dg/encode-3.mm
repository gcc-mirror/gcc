/* { dg-do run } */

extern "C" {
extern void abort (void);
extern int strcmp (const char *, const char *);
}

template <class T>
struct Vec {
  T x, y;
  long z;
  long long zz;
};

typedef struct {
  float fscalar;
  double dv[10];
  int iscalar;
  long z;
  long long zz;
  Vec<const signed char> cv;
} anonymous;

//Vec<double> dd;
const char *enc = @encode(Vec<float>);
const char *enc2 = @encode(Vec<double>);
const char *enc3 = @encode(anonymous);

#ifdef __LP64__
#define L "q"
#else
#define L "l"
#endif

int main(void) {
  const char *encode = @encode(long);

  if (strcmp (encode, L))
    abort();

  if (strcmp (enc, "{Vec<float>=ff" L "q}"))
    abort();

  if (strcmp (enc2, "{Vec<double>=dd" L "q}"))
    abort();

  if (strcmp (enc3, "{?=f[10d]i" L "q{Vec<const signed char>=rcrc" L "q}}"))
    abort();

  return 0;
}
