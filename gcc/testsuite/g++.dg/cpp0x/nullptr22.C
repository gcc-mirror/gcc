// { dg-do compile }
// { dg-options "-std=c++0x -Wall -Wformat=2 -Wstrict-null-sentinel" }

// Test various warnings

void f1(const char*, ...) __attribute__((format(printf, 1, 2)));
void f2(const char*) __attribute__((nonnull));
void f3(const char*, ...) __attribute__((sentinel));

void f()
{
  f1("%p", nullptr);
  f2(nullptr); // { dg-warning "null argument where non-null required " }
  f3("x", "y", __null); // { dg-warning "missing sentinel in function call" }
  f3("x", "y", nullptr);
  decltype(nullptr) mynull = 0;
  f1("%p", mynull);
  f2(mynull); // { dg-warning "null argument where non-null required " }
  f3("x", "y", mynull);
}
