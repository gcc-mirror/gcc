// PR c++/37530
// { dg-do compile }

void
foo (bool b)
{
  if (b)
    try { throw 0; } catch (X) { }	// { dg-error "type" }
}
