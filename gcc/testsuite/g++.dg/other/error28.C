// PR c++/35987
// { dg-do compile }

void
foo (char *p)
{
  if (++p = true);	// { dg-error "cannot convert" }
}
