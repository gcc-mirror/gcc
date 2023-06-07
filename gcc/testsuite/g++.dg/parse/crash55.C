// PR c++/42038

// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

extern int __cxa_begin_catch;	// { dg-message "previous declaration" }

void f(void)
{
  try { } catch (int) { }  // { dg-error "redeclared"  }
}
