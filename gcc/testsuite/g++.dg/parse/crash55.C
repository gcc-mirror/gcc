// PR c++/42038

extern int __cxa_begin_catch;	// { dg-message "previous declaration" }

void f(void)
{
  try { } catch (int) { }  // { dg-error "redeclared"  }
}
