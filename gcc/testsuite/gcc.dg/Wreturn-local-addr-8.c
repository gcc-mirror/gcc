/* Test to verify that a MAX_EXPR and MIN_EXPR in a return statement
   is handled correctly and that all local variables whose address
   is or may be returned are identified.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char* sink (char*, ...);

void* test_max_2 (void)
{
  char c;                     /* { dg-message "declared here" } */

  char *p = sink (&c);

  void *q = p > &c ? p : &c;  /* MAX_EXPR */
  return q;                   /* { dg-warning "\\\[-Wreturn-local-addr" } */
}

void* test_max_3 (void)
{
  char c;                     /* { dg-message "declared here" } */
  char d;                     /* { dg-message "declared here" } */

  char *p = sink (&c, &d);

  void *q = p < &c ? &c < &d ? &d : &c : p;
  return q;                   /* { dg-warning "\\\[-Wreturn-local-addr" } */
}

void* test_min_2 (void)
{
  char c;                     /* { dg-message "declared here" } */

  char *p = sink (&c);

  void *q = p < &c ? p : &c;  /* MIN_EXPR" */
  return q;                   /* { dg-warning "\\\[-Wreturn-local-addr" } */
}

void* test_min_3 (void)
{
  char c;                     /* { dg-message "declared here" } */
  char d;                     /* { dg-message "declared here" } */

  char *p = sink (&c, &d);

  void *q = p > &c ? &c > &d ? &d : &c : p;
  return q;                   /* { dg-warning "\\\[-Wreturn-local-addr" } */
}

void* test_min_2_phi (int i)
{
  char a;                     /* { dg-message "declared here" } */

  char *p = &a;
  char *q = sink (&a);
  p = p < q ? p : q;
  if (i == 1)
    return p;
  /* { dg-warning "may return address of local variable" "missing location" { xfail *-*-* } } */
  else
    return q;
}

void* test_min_3_phi (int i)
{
  char a;                     /* { dg-message "declared here" } */
  char b;                     /* { dg-message "declared here" } */

  char *p0 = &a;
  char *p1 = &b;
  char *p2 = sink (&a, &b);
  char *p3 = sink (&a, &b);

  char *p4 = p2 < p0 ? p2 : p0;
  char *p5 = p3 < p1 ? p3 : p1;

  if (i == 1)
    /* { dg-warning "may return address of local variable" "missing location" { xfail *-*-* } } */
    return p4;
  else
    /* { dg-warning "may return address of local variable" "missing location" { xfail *-*-* } } */
    return p5;
}

/* The directive below "swallows" warnings for both test_min_2_phi
   and test_min_3_phi.
  { dg-warning "may return address of local variable" "pr90735" { target *-*-* } 0 } */
