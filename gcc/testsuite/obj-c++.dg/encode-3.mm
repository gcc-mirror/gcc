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

/* Darwin (at least, as of XCode 3.2.3/Darwin10) does not encode the read-only
   attribute  on the type.  Arguably, this is a bug, but we are compatible
   with this when -fnext-runtime is selected.  */
#ifdef __NEXT_RUNTIME__
#define E3 "{?=f[10d]i" L "q{Vec<const signed char>=cc" L "q}}"
#else
#define E3 "{?=f[10d]i" L "q{Vec<const signed char>=rcrc" L "q}}"
#endif

int main(void) {
  const char *encode = @encode(long);

  if (strcmp (encode, L))
    abort ();

  if (strcmp (enc, (const char *)"{Vec<float>=ff" L "q}"))
    abort ();

  if (strcmp (enc2, (const char *)"{Vec<double>=dd" L "q}"))
    abort ();

  if (strcmp (enc3, (const char *) E3))
    abort ();

  return 0;
}
