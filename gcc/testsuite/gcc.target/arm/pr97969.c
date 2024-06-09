/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6m_ok } */
/* { dg-options "-std=c99 -fno-omit-frame-pointer -w -Os" } */
/* { dg-add-options arm_arch_v6m } */

typedef int a[23];
enum { b };
typedef struct {
  int c;
  char *e;
  char f;
} d;
typedef enum { g = 1 } h;
typedef struct {
  h i;
  int j;
} k;
typedef struct {
  a l;
  int a;
  int m;
  int n;
  int o;
  short p;
  int q;
  k r;
  char e;
  char *s;
  d t;
  d *u;
  short v;
  int w;
} aa;

void ae(short*, int, int);
void af(aa*, int, char, int);
int ag(int);
void ah(aa);
void ai(int);
void aj(aa);
int setjmp();

int c(char x, int y, char z, int ab) {
  aa ac;
  ac.r.i = 0;
  d ad;
  ac.t = ad;
  ac.u = 0;
  ae(&ac.v, 0, 0);
  ac.w = 0;
  af(&ac, x + y, z, z + ab);
  if (ag(0))
    return 0;
  if (x)
    ac.s = (char*)(z + ab);
  else
    ac.s = (char*)(x + y);
  ac.o |= g;
  if (!setjmp()) {
    ah(ac);
    ai(b);
    ac.e = z + ab;
    aj(ac);
  }
}
