// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" int memcmp (const void * __s1,
		       const void * __s2,
		       __SIZE_TYPE__ __n) throw ();

namespace std {
void f () {
  memcmp (0, 0, 0);
}
}
