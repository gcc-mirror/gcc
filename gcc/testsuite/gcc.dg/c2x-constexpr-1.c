/* Test C2x constexpr.  Valid code, compilation tests.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#include <float.h>

constexpr int v1 = 1;
static_assert (v1 == 1);
extern typeof (v1) *pci;
extern const int *pci;
extern typeof (&(constexpr int) {}) pci;
/* Redeclaring a constexpr object is OK (although it can't be declared before
   the definition without undefined behavior).  */
extern const int v1;
static_assert (v1 == 1);
unsigned int constexpr v2 = 2;
static_assert (v2 == 2);
extern typeof (v2) *pcui;
extern const unsigned int *pcui;
static constexpr char v3 = 3;
static_assert (v3 == 3);
extern typeof (v3) *pcc;
extern const char *pcc;
constexpr void *v4 = 0;
extern typeof (v4) *pcpv;
extern void *const *pcpv;
constexpr int *v5 = nullptr;
extern typeof (v5) *pcpi;
extern int *const *pcpi;
constexpr double v6 = 3.5;
extern typeof (v6) *pcd;
extern const double *pcd;
auto constexpr v7 = 1.0;
extern typeof (v7) *pcd;
constexpr auto v8 = 1.5f;
extern typeof (v8) *pcf;
extern const float *pcf;
constexpr static long v9 = 2ULL;
static_assert (v9 == 2);
extern typeof (v9) *pcl;
extern const long *pcl;
const short *v10 = &(constexpr short) { 10 };
/* Qualifiers that aren't permitted on a constexpr object itself are OK in a
   pointer target.  */
constexpr volatile int *v11 = nullptr;
extern typeof (v11) *pcpvi;
extern volatile int *const *pcpvi;
constexpr _Atomic int *v12 = nullptr;
extern typeof (v12) *pcpai;
extern _Atomic int *const *pcpai;
constexpr int *restrict *v13 = nullptr;
extern typeof (v13) cprpi;
extern int *restrict *const cprpi;
typedef int *P;
constexpr restrict P *v14 = nullptr;
extern typeof (v14) cprpi;
struct s15 { volatile int a; _Atomic int b; int *restrict p; };
constexpr struct s15 *v16 = nullptr;
constexpr char v17[3] = { 1, 2, 3 };
struct s18 { int a; int *b; double c; };
constexpr struct s18 v19 = { 12345ULL, 0, 19.0L };
static_assert (v19.a == 12345);
union u20 { int a; float b; };
constexpr union u20 v21 = { 1 };
static_assert (v21.a == 1);
constexpr union u20 v22 = { .b = 23.0 };
constexpr float v23 = (float) (1.0f / 3.0f);
constexpr double v24 = (double) (1.0 / 3.0);
constexpr struct s18 v25 = { 0, 0, (double) (1.0 / 3.0) };
static_assert (v25.a == 0);
constexpr char v26[] = "abc\xfe";
constexpr unsigned char v27[] = u8"xyz\xff";
constexpr unsigned char v28[] = "\x12\x7f";
constexpr signed char v29[] = "\x34\x66";
constexpr double v30 = (int) (double) 3.0 - (long) (double) 2.0;
constexpr int v31 = 1 + 2 + (int) 3.0;
static_assert (v31 == 6);
constexpr typeof (nullptr) v32 = nullptr;
constexpr _Complex double v33 = __builtin_complex (1.0f, 3.0f / 2.0f);
constexpr float v34 = 1234.0L;
constexpr char v35 = 127ULL;
#if FLT_MIN_EXP == -125 && FLT_MANT_DIG == 24
constexpr float v36 = 0x1p-149;
constexpr float _Complex v37 = __builtin_complex (0x1p-149, 0x1p127);
constexpr float v38 = 0xffffffUL;
constexpr float v39 = -0xffffffL;
constexpr float v40 = 0xffffff0L;
constexpr float v41 = 1ULL << 63;
#endif
#if DBL_MIN_EXP == -1021 && DBL_MANT_DIG == 53
constexpr double v42 = 0x1p-1074L;
constexpr _Complex double v43 = __builtin_complex (0x1p1023L, 0x1p-1074L);
constexpr double v44 = 0x1fffffffffffffULL;
constexpr double v45 = -0x1fffffffffffffLL;
constexpr double v46 = 0x3ffffffffffffeULL;
constexpr double v47 = 1ULL << 63;
#endif
constexpr void *v48 = (void *) 0;
constexpr int *v49 = (void *) 0L;
constexpr long *v50 = 0LL;
constexpr int v51 = {};
static_assert (v51 == 0);
constexpr float v52 = {};
constexpr long double v53 = {};
constexpr int *v54 = {};
constexpr void *v55 = {};
constexpr typeof (nullptr) v56 = {};
struct s57 { int *p; };
union u58 { int *p; };
constexpr int *v59 = 0;
constexpr int *v60 = { 0 };
constexpr struct s57 v61 = { 0 };
constexpr struct s57 v62 = { { } }; /* { dg-warning "braces around scalar initializer" } */
constexpr struct s57 v63 = { { 0 } }; /* { dg-warning "braces around scalar initializer" } */
constexpr union u58 v64 = { 0 };
constexpr union u58 v65 = { { } }; /* { dg-warning "braces around scalar initializer" } */
constexpr union u58 v66 = { { 0 } }; /* { dg-warning "braces around scalar initializer" } */
struct s67 { int a; float b; void *c; int *d; typeof (nullptr) e; int f; int g[2]; };
struct s68 { struct s67 x; };
union u69 { int a; float b; void *c; int *d; struct s68 e; };
struct s70 { union u69 x; };
constexpr struct s67 v71 = { 1, 2.0, 0, 0, nullptr, 7, { 3, 4 } };
static_assert (v71.a == 1);
static_assert (v71.f == 7);
constexpr struct s67 v72 = v71;
static_assert (v72.a == 1);
static_assert (v72.f == 7);
extern const struct s67 v71;
constexpr auto v73 = v71;
static_assert (v73.a == 1);
static_assert (v73.f == 7);
auto v74 = v71;
constexpr struct s68 v75 = { v72 };
static_assert (v75.x.a == 1);
static_assert (v75.x.f == 7);
constexpr union u69 v76 = { };
static_assert (v76.a == 0);
constexpr union u69 v77 = { .e = v75 };
static_assert (v77.e.x.a == 1);
static_assert (v77.e.x.f == 7);
constexpr union u69 v78 = { .a = 1 };
static_assert (v78.a == 1);
constexpr union u69 v79 = { .e = { v72 } };
static_assert (v79.e.x.a == 1);
static_assert (v79.e.x.f == 7);
enum e80 { E80 = v79.e.x.f };
static_assert (E80 == 7);
constexpr struct s70 v81 = { v79 };
static_assert (v81.x.e.x.f == 7);
constexpr struct s68 v82 = { (constexpr struct s67) { 5, 6, 0, 0, nullptr, 9, { 1, 2 } } };
static_assert (v82.x.a == 5);
static_assert (v82.x.f == 9);
constexpr auto v83 = (constexpr int) { (constexpr int) { 0 } };
/* These are null pointers but not null pointer constants.  */
constexpr typeof (nullptr) v84 = nullptr;
constexpr void *v85 = 0;
int *v86 = v85;
int *v87 = v84;
typeof (1 ? v85 : (int *) 0) v88;
extern void *v88;
typeof (1 ? (void *) 0 : (int *) 0) v89;
extern int *v89;
constexpr struct s68 v90 = { };
static_assert (v90.x.a == 0);
static_assert (v90.x.f == 0);
constexpr int v91 = { 123 };
static_assert (v91 == 123);
constexpr int v92 = { v91 };
static_assert (v92 == 123);
/* Verify that constexpr values can be used in various contexts requiring
   (integer) constant expressions.  */
struct s93 { int x : v79.e.x.f; };
constexpr int v94 = alignof (int);
alignas (v94) int v95;
constexpr int v97[100] = { [v82.x.f] = 7 };
static int v98[v94];
constexpr _Complex double v99 = 1.0;
constexpr _Complex float v100 = 12345;
constexpr int *v101 = (int *) 0;
constexpr void *v102 = (void *) (void *) 0;
constexpr void *v103 = v101;
constexpr void *v104 = v84;
struct s105 { void *p; };
constexpr struct s105 v106 = { (int *) 0 };

void
f0 ()
{
  constexpr int fv0 = 3;
  static_assert (fv0 == 3);
  auto constexpr fv1 = 4;
  static_assert (fv1 == 4);
  register constexpr float fv2 = 1.0;
  constexpr auto fv3 = 123;
  static_assert (fv3 == 123);
  constexpr register void *fv4 = (void *) 0;
  const int *fv5 = &(constexpr int) { 234 };
  const int *fv6 = &(constexpr static int) { 234 };
  const int *fv7 = &(static constexpr int) { 234 };
  typeof ((constexpr register int) { 234 }) *fv8;
  typeof ((register constexpr int) { 234 }) *fv9;
  int fv10 = (constexpr int) { 1 } + sizeof (struct fs *);
  constexpr auto fv11 = (constexpr int) { (constexpr int) { 0 } };
  static_assert (fv11 == 0);
  constexpr char fv12[3] = { 1, 2, 3 };
  (constexpr short [4]) { 9, 8, 7, -6 };
  constexpr struct s18 fv13 = { 1234ULL, 0, 13.0f };
  (constexpr struct s18) { 123, (void *) 0, 11 };
  constexpr union u20 fv14 = { 2 };
  (constexpr union u20) { 5 };
  constexpr union u20 fv15 = { .b = 15.0 };
  (constexpr union u20) { .b = 20 };
  (constexpr float) { (float) (1.0f / 3.0f) };
  (constexpr double) { (double) (1.0 / 3.0) };
  (constexpr struct s18) { 0, 0, (double) (1.0 / 3.0) };
  (constexpr char []) { "abc\xfe" };
  (constexpr unsigned char []) { u8"xyz\xff" };
  (constexpr unsigned char []) { "\x12\x7f" };
  (constexpr signed char []) { "\x34\x66" };
  (constexpr double) { (int) (double) 3.0 - (long) (double) 2.0 };
  (constexpr int) { 1 + 2 + (int) 3.0 };
  (constexpr typeof (nullptr)) { nullptr };
  (constexpr _Complex double) { __builtin_complex (1.0f, 3.0f / 2.0f) };
  (constexpr float) { 1234.0L };
  (constexpr char) { 127ULL };
#if FLT_MIN_EXP == -125 && FLT_MANT_DIG == 24
  (constexpr float) { 0x1p-149 };
  (constexpr float _Complex) { __builtin_complex (0x1p-149, 0x1p127) };
  (constexpr float) { 0xffffffUL };
  (constexpr float) { -0xffffffL };
  (constexpr float) { 0xffffff0L };
  (constexpr float) { 1ULL << 63 };
#endif
#if DBL_MIN_EXP == -1021 && DBL_MANT_DIG == 53
  (constexpr double) { 0x1p-1074L };
  (constexpr _Complex double) { __builtin_complex (0x1p1023L, 0x1p-1074L) };
  (constexpr double) { 0x1fffffffffffffULL };
  (constexpr double) { -0x1fffffffffffffLL };
  (constexpr double) { 0x3ffffffffffffeULL };
  (constexpr double) { 1ULL << 63 };
#endif
  (constexpr void *) { (void *) 0 };
  (constexpr int *) { (void *) 0L };
  (constexpr long *) { 0LL };
  (constexpr int) {};
  (constexpr float) {};
  (constexpr long double) {};
  (constexpr int *) {};
  (constexpr void *) {};
  (constexpr typeof (nullptr)) {};
  (constexpr int *) { 0 };
  (constexpr struct s57) { 0 };
  (constexpr struct s57) { { } }; /* { dg-warning "braces around scalar initializer" } */
  (constexpr struct s57) { { 0 } }; /* { dg-warning "braces around scalar initializer" } */
  (constexpr union u58) { 0 };
  (constexpr union u58) { { } }; /* { dg-warning "braces around scalar initializer" } */
  (constexpr union u58) { { 0 } }; /* { dg-warning "braces around scalar initializer" } */
  (constexpr _Complex double) { 1.0 };
  (constexpr _Complex float) { 12345 };
  (constexpr int *) { (int *) 0 };
  (constexpr void *) { (void *) (void *) 0 };
  (constexpr void *) { v101 };
  (constexpr void *) { v84 };
  (constexpr struct s105) { (int *) 0 };
  /* It's not entirely clear if constexpr declarations are allowed in this
     position in a for loop; presume they are, as implicitly auto just as if no
     storage class specifiers were used.  */
  for (constexpr int fv16 = 1;;)
    break;
  constexpr struct s67 fv17 = { 1, 2.0, 0, 0, nullptr, 7, { 3, 4 } };
  static_assert (fv17.a == 1);
  static_assert (fv17.f == 7);
  constexpr struct s67 fv18 = fv17;
  static_assert (fv18.a == 1);
  static_assert (fv18.f == 7);
  constexpr auto fv19 = fv17;
  static_assert (fv19.a == 1);
  static_assert (fv19.f == 7);
  auto fv20 = fv17;
  constexpr struct s68 fv21 = { fv18 };
  static_assert (fv21.x.a == 1);
  static_assert (fv21.x.f == 7);
  constexpr union u69 fv22 = { };
  static_assert (fv22.a == 0);
  constexpr union u69 fv23 = { .e = fv21 };
  static_assert (fv23.e.x.a == 1);
  static_assert (fv23.e.x.f == 7);
  constexpr union u69 fv24 = { .a = 1 };
  static_assert (fv24.a == 1);
  constexpr union u69 fv25 = { .e = { fv18 } };
  static_assert (fv25.e.x.a == 1);
  static_assert (fv25.e.x.f == 7);
  enum fe80 { FE80 = fv25.e.x.f };
  static_assert (FE80 == 7);
  constexpr struct s70 fv26 = { fv25 };
  static_assert (fv26.x.e.x.f == 7);
  constexpr struct s68 fv27 = { (constexpr struct s67) { 5, 6, 0, 0, nullptr, 9, { 1, 2 } } };
  static_assert (fv27.x.a == 5);
  static_assert (fv27.x.f == 9);
  constexpr struct s68 fv28 = { };
  static_assert (fv28.x.a == 0);
  static_assert (fv28.x.f == 0);
  constexpr int fv29 = { 123 };
  static_assert (fv29 == 123);
  constexpr int fv30 = { fv29 };
  static_assert (fv30 == 123);
  static_assert ((constexpr struct s67) { 1, 2.0, 0, 0, nullptr, 7, { 3, 4 } }.f == 7);
  static_assert ((constexpr struct s68) { fv18 }.x.a == 1);
  static_assert ((constexpr union u69) { }.a == 0);
  static_assert ((constexpr union u69) { .e = fv21 }.e.x.f == 7);
  static_assert ((constexpr union u69) { .a = 1 }.a == 1);
  static_assert ((constexpr union u69) { .e = { fv18 } }.e.x.a == 1);
  static_assert ((constexpr struct s70) { fv25 }.x.e.x.f == 7);
  static_assert ((constexpr struct s68) { (constexpr struct s67) { 5, 6, 0, 0, nullptr, 9, { 1, 2 } } }.x.f == 9);
  static_assert ((constexpr struct s68) { }.x.f == 0);
  /* Verify that constexpr values can be used in various contexts requiring
     (integer) constant expressions.  */
  struct fs93 { int x : fv25.e.x.f; };
  constexpr int fv31 = alignof (int);
  alignas (fv31) int fv32;
  constexpr int fv33[100] = { [fv27.x.f] = 7 };
  static int fv34[fv31];
  switch (fv0)
    {
    case fv27.x.f: ;
    }
}
