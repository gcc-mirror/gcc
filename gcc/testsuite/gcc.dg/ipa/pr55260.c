/* { dg-do compile } */
/* { dg-options "-O2 -fno-inline -fipa-cp-clone"  } */

typedef struct {
  int *ptr;
  int len;
} string;
typedef struct {
  string nantstr;
  int *nant;
} malv;
typedef struct {
  int *nor;
} list_heads;
int b;
list_heads *fn1(string, int *, unsigned);
void fn2(malv *p1, list_heads *p2, unsigned p3) {
  string a = p1->nantstr;
  fn1(a, p1->nant, p3);
}

void fn3(unsigned p1) { fn2(0, 0, p1); }

list_heads *fn1(string p1, int *p2, unsigned p3) {
  while (1) {
    if (p3)
      fn3(1);
    if (b)
      return 0;
    fn3(1);
  }
}

void fn5() {
  list_heads c;
  c.nor = 0;
  fn2(0, &c, 1);
}
