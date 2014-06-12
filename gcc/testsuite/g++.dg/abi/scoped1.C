// { dg-do run { target c++11 } }
// { dg-options "-fabi-version=0 -Wabi=2" }

enum class A: short { a1, a2, a3 };
void f(int i, ...)
{
  __builtin_va_list ap;
  __builtin_va_start (ap, i);
  if (__builtin_va_arg (ap, A) != A::a1) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a2) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a3) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a1) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a2) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a3) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a1) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a2) __builtin_abort(); // { dg-warning "passed" }
  if (__builtin_va_arg (ap, A) != A::a3) __builtin_abort(); // { dg-warning "passed" }
}

int main()
{
  f(9, A::a1, A::a2, A::a3, A::a1, A::a2, A::a3, A::a1, A::a2, A::a3);	// { dg-warning "passed" }
}
