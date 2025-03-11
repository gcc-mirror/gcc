/* { dg-do compile } */
/* { dg-options "-w" } */

extern void abort(void);
extern long long strcspn(const char *, const char *);

void main_test(void) {
  const char *const s1 = "hello world";
  char dst[64], *d2;

  if (strcspn(++d2 + 5, "") != 5 || d2 != dst + 1)
    abort();
}
