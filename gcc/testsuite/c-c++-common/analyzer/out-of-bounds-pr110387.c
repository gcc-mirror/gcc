char a, b, c, d;
long x;

void
_S_copy (long __n)
{
  __builtin_memcpy (&a, &d, __n); /* { dg-prune-output "-Wanalyzer-out-of-bounds" } */
  /* This only warns on some targets; the purpose of the test is to verify that
     we don't ICE.  */
}

void
_M_construct ()
{
  x = &c - &b; /* { dg-warning "undefined behavior when subtracting pointers" } */
  unsigned long __dnew = x;
  if (__dnew > 1)
    _S_copy (&c - &b); /* { dg-warning "undefined behavior when subtracting pointers" } */
}
