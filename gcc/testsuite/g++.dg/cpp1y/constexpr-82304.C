// PR c++/82304
// { dg-do compile { target c++14 } }

typedef __UINTPTR_TYPE__ uintptr_t;

constexpr const char *
foo (const char *p)
{
  auto l = reinterpret_cast<uintptr_t>(p);	// { dg-error "conversion from pointer" }
  ++l;
  return reinterpret_cast<const char *>(l);
}

constexpr auto s = foo ("Hello");
