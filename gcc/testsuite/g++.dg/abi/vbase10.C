// { dg-do compile }
// { dg-options "-Wabi -fabi-version=1" }

struct A { virtual void f(); char c1; };
struct B { B(); char c2; };
// On ARM processors, the alignment of B will be 4 even though it
// contains only a single "char".  That avoids the situation that the
// warning below is designed to catch.  On ARM NetBSD, the alignment
// of B will be only 1 -- but there is no way to tell DejaGNU that a
// failure is expected on all ARM targets except arm*-*-netbsd*.
// Therefore, this test will XPASS on arm*-*-netbsd*.
struct C : public A, public virtual B {}; // { dg-warning "ABI" "" { xfail arm*-*-* } }

