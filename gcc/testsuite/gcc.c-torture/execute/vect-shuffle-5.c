/* Test that different type variants are compatible within
   vector shuffling.  */

#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define shufcompare(count, vres, v0, mask) \
do { \
    int __i; \
    for (__i = 0; __i < count; __i++) { \
        if (vres[__i] != v0[mask[__i]]) \
            __builtin_abort (); \
    } \
} while (0)

#define test_compat_mask(res, vec, mask) \
  res = __builtin_shuffle (vec, mask); \
  shufcompare(4, res, vec, mask); \
  res = __builtin_shuffle (vec, c ## mask); \
  shufcompare(4, res, vec, c ##  mask); \
  res = __builtin_shuffle (vec, r ## mask); \
  shufcompare(4, res, vec, r ##  mask); \
  res = __builtin_shuffle (vec, d ## mask); \
  shufcompare(4, res, vec, d ##  mask); \
  res = __builtin_shuffle (vec, dc ## mask); \
  shufcompare(4, res, vec, dc ##  mask); \

#define test_compat_vec(res, vec, mask) \
  test_compat_mask (res, vec, mask); \
  test_compat_mask (res, c ## vec, mask); \
  test_compat_mask (res, r ## vec, mask); \
  test_compat_mask (res, d ## vec, mask); \
  test_compat_mask (res, dc ## vec, mask);

#define test_compat(res, vec, mask) \
  test_compat_vec (res, vec, mask); \
  test_compat_vec (d ## res, vec, mask); \
  test_compat_vec (r ## res, vec, mask);

typedef vector (4, int) v4si;
typedef const vector (4, int) v4sicst;

int main (int argc, char *argv[]) {
    vector (4, int) vec = {argc, 1,2,3};
    const vector (4, int) cvec = {argc, 1,2,3};
    register vector (4, int) rvec = {argc, 1,2,3};
    v4si dvec = {argc, 1,2,3};
    v4sicst dcvec = {argc, 1,2,3};

    vector (4, int) res;
    v4si dres;
    register vector (4, int) rres;

    vector (4, int) mask = {0,3,2,1};
    const vector (4, int) cmask = {0,3,2,1};
    register vector (4, int) rmask = {0,3,2,1};
    v4si dmask = {0,3,2,1};
    v4sicst dcmask = {0,3,2,1};

    test_compat (res, vec, mask);

    return 0;
}


