// PR c++/51930
// { dg-require-visibility "" }
// { dg-options -fvisibility=hidden }
// { dg-final { scan-not-hidden "_Z8testfuncI3fooEvv" } }

struct foo { };

template<typename T>
__attribute__ ((visibility("default")))
void testfunc();

template<typename T> void testfunc() { }

template
__attribute__ ((visibility("default")))
void testfunc<foo>();
