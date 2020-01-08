/* Test handling of lvalues of incomplete types.  Bugs 36941, 88647
   (invalid), 88827.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

struct S;

extern struct S var;
extern struct S *vp;
extern int i;

void
f1 (void)
{
  var; /* { dg-error "has an incomplete type" } */
  var, (void) 0; /* { dg-error "has an incomplete type" } */
  (i
   ? var /* { dg-error "has an incomplete type" } */
   : var); /* { dg-error "has an incomplete type" } */
}

void
f2 (void)
{
  (void) var; /* { dg-error "has an incomplete type" } */
  (void) (var, (void) 0); /* { dg-error "has an incomplete type" } */
  (void) (i
	  ? var /* { dg-error "has an incomplete type" } */
	  : var); /* { dg-error "has an incomplete type" } */
}

void
f3 (void)
{
  (const void) var; /* { dg-error "has an incomplete type" } */
  (const void) (var, (void) 0); /* { dg-error "has an incomplete type" } */
  (const void) (i
		? var /* { dg-error "has an incomplete type" } */
		: var); /* { dg-error "has an incomplete type" } */
}

void
f4 (void)
{
  *vp; /* { dg-error "invalid use of undefined type" } */
  *vp, (void) 0; /* { dg-error "invalid use of undefined type" } */
  (i
   ? *vp /* { dg-error "invalid use of undefined type" } */
   : *vp); /* { dg-error "invalid use of undefined type" } */
}

void
f5 (void)
{
  (void) *vp; /* { dg-error "invalid use of undefined type" } */
  (void) (*vp, (void) 0); /* { dg-error "invalid use of undefined type" } */
  (void) (i
	  ? *vp /* { dg-error "invalid use of undefined type" } */
	  : *vp); /* { dg-error "invalid use of undefined type" } */
}

void
f6 (void)
{
  (const void) *vp; /* { dg-error "invalid use of undefined type" } */
  (const void) (*vp, (void) 0); /* { dg-error "invalid use of undefined type" } */
  (const void) (i
		? *vp /* { dg-error "invalid use of undefined type" } */
		: *vp); /* { dg-error "invalid use of undefined type" } */
}

void
f7 (void)
{
  /* This is invalid because of the constraints on [].  */
  &vp[0]; /* { dg-error "invalid use of undefined type" } */
}
