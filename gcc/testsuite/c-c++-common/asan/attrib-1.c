/* PR sanitizer/55435 */
/* { dg-do compile } */

__attribute__((no_sanitize_address)) int
f1 (int *p, int *q)
{
  *p = 42;
  return *q;
}

void f2 (char *);
void f2 (char *) __attribute__((no_sanitize_address));
void f2 (char *) __attribute__((no_sanitize_address));
void f2 (char *);

void
f2 (char *p)
{
  *p = 42;
}

void f3 (short *);
__typeof (f3) f3  __attribute__((__no_sanitize_address__));

void
f3 (short *p)
{
  *p = 42;
}

__attribute__((no_sanitize_address)) int
f4 (int *p, int *q)
{
  *p = 42;
  return *q;
}

void f5 (char *);
void f5 (char *) __attribute__((no_sanitize_address));
void f5 (char *) __attribute__((no_sanitize_address));
void f5 (char *);

void
f5 (char *p)
{
  *p = 42;
}

void f6 (short *);
__typeof (f6) f6  __attribute__((__no_address_safety_analysis__));

void
f6 (short *p)
{
  *p = 42;
}

int v __attribute__((no_sanitize_address)); /* { dg-warning "attribute ignored" } */
int v2 __attribute__((no_address_safety_analysis)); /* { dg-warning "attribute ignored" } */

/* { dg-final { scan-assembler-not "__asan_report_store" } } */
/* { dg-final { scan-assembler-not "__asan_report_load" } } */
