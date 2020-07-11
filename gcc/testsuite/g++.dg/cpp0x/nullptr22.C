// { dg-do compile { target c++11 } }
// { dg-options "-Wall -Wformat=2 -Wstrict-null-sentinel" }

// Test various warnings

void f1(const char*, ...) __attribute__((format(printf, 1, 2)));
void f2(const char*) __attribute__((nonnull));
void f3(const char*, ...) __attribute__((sentinel));

void f()
{
  f1("%p", nullptr);
  f2(nullptr); // { dg-warning "argument 1 null where non-null expected " }
  f3("x", "y", __null); // { dg-warning "missing sentinel in function call" }
  f3("x", "y", nullptr);
  decltype(nullptr) mynull = 0;
  f1("%p", mynull);
  f2(mynull); // { dg-warning "argument 1 null where non-null expected " }
  f3("x", "y", mynull);
}
