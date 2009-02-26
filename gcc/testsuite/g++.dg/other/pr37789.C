// PR c++/37789
// { dg-do compile }

void foo():
{		// { dg-error "initializers|identifier" }
  __FUNCTION__;
}
