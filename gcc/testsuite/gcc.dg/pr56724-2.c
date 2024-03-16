/* PR c/56724 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -Wc++-compat -Wpedantic" } */

enum E1 { A };
enum E2 { B };
extern void foo_E (enum E1);
extern void foo_v (void *p);
extern void foo_sc (int, int, signed char *);
extern unsigned char *uc;
extern signed char sc;
extern const signed char *csc;
extern float *f;

void
foo (void)
{
  void (*fp)(void);
  const void (*ffp)(void);
  foo_v (fp); /* { dg-warning "10:ISO C forbids passing argument" } */
  foo_E (B); /* { dg-warning "10:enum conversion when passing argument" } */
  foo_sc (1, 2, uc); /* { dg-warning "17:pointer targets in passing argument" } */
  foo_sc (1, 2, f); /* { dg-warning "17:passing argument" } */
  foo_sc (1, 2, sc); /* { dg-warning "17:passing argument" } */
  foo_sc (uc, 2, &sc); /* { dg-warning "11:passing argument" } */
  foo_sc (1, 2, csc); /* { dg-warning "17:passing argument" } */
}

typedef void (*fp)(void);
typedef void (*nrfp)(void) __attribute__((noreturn));
void f1 (nrfp); void f2 (fp x) { f1 (x); } extern int e; /* { dg-warning "38:passing argument" } */
