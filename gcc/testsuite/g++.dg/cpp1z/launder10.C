// PR c++/116673
// { dg-do compile }

void
bar (void *p)
{
  __builtin_launder (bar); // { dg-error {argument to '__builtin_launder'} }
  __builtin_launder (p);   // { dg-error {argument to '__builtin_launder'} }
  const void* cp = p;
  __builtin_launder (cp);  // { dg-error {argument to '__builtin_launder'} }
  volatile void* vp = p;
  __builtin_launder (vp);  // { dg-error {argument to '__builtin_launder'} }
  const volatile void* cvp = p;
  __builtin_launder (cvp); // { dg-error {argument to '__builtin_launder'} }
}
