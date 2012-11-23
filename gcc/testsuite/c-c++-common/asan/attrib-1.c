/* PR sanitizer/55435 */
/* { dg-do compile } */

__attribute__((no_address_safety_analysis)) int
f1 (int *p, int *q)
{
  *p = 42;
  return *q;
}

void f2 (char *);
void f2 (char *) __attribute__((no_address_safety_analysis));
void f2 (char *) __attribute__((no_address_safety_analysis));
void f2 (char *);

void
f2 (char *p)
{
  *p = 42;
}

void f3 (short *);
__typeof (f3) f3  __attribute__((__no_address_safety_analysis__));

void
f3 (short *p)
{
  *p = 42;
}

int v __attribute__((no_address_safety_analysis)); /* { dg-warning "attribute ignored" } */

/* { dg-final { scan-assembler-not "__asan_report_store" } } */
/* { dg-final { scan-assembler-not "__asan_report_load" } } */
