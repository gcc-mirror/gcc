/* Test for format extensions beyond the C standard and X/Open standard.
   Test for scanf formats.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

/* %q formats want a "quad"; GCC considers this to be a long long.  */
typedef long long int quad_t;
typedef unsigned long long int u_quad_t;

typedef __WCHAR_TYPE__ wchar_t;

extern int scanf (const char *, ...);

void
foo (quad_t *qp, u_quad_t *uqp, quad_t *qn, long long int *llp,
     unsigned long long int *ullp, float *fp, char *s, void **pp, wchar_t *ls)
{
  /* As an extension, GCC allows the BSD length "q" for integer formats.
     This is largely obsoleted in C99 by %j, %ll and SCNd64.
  */
  scanf ("%qd%qi%qo%qu%qx%qX%qn", qp, qp, uqp, uqp, uqp, uqp, qn);
  scanf ("%qf", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qF", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qe", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qE", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qg", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qG", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qa", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qA", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qs", s); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%q[abc]", s); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qc", s); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qp", pp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qC", ls); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qS", ls); /* { dg-warning "length" "bad use of %q" } */
  /* As an extension, GCC allows the length "L" on integer formats
     (but not %n) as a synonym for "ll".
     This should be considered deprecated.
  */
  scanf ("%Ld%Li%Lo%Lu%Lx%LX", llp, llp, ullp, ullp, ullp, ullp);
  /* glibc also supports flags ' and I on scanf formats, but GCC
     doesn't yet.  */
}
