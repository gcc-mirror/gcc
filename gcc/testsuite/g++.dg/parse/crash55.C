// PR c++/42038

extern int __cxa_begin_catch;	// { dg-error "declared incorrectly" }

void f(void)
{
  try { } catch (int) { }
}
