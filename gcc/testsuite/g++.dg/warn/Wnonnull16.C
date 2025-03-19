// PR c++/115580
// { dg-do compile { target c++11 } }

class WithMember {
public:
  int foo();
};

decltype(((WithMember*)nullptr)->foo()) footype; // { dg-bogus "pointer is null" }

int f(void*) __attribute__((nonnull));

void g()
{
  [[maybe_unused]] decltype(f(nullptr)) b; // { dg-bogus "non-null" }
}
