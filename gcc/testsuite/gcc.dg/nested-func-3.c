/* Undefined nested function should be a error, whether or not the
   function is called.  Bug 17807.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (void)
{
  auto int fn (int); /* { dg-error "nested function 'fn' declared but never defined" } */
  auto int fn2 (int); /* { dg-error "nested function 'fn2' declared but never defined" } */
  sizeof(fn(1));
}

void
h (void)
{
  auto int hn (int); /* { dg-error "nested function 'hn' declared but never defined" } */
  hn (1);
}
