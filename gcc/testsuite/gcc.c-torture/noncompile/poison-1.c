#pragma poison foo
foo
#pragma poison foo2 foo3
foo2
foo3
#pragma   poison	foo4 	foo5
foo4
foo5
#pragma poison +++
#define foo6 123
#pragma poison foo6
#define foo6 345
#define foo6 456
#ifdef foo6
#error hey!  foo6 poisoned!
#endif
#if defined(foo6)
#error no, foo6 still poisoned!
#else
foo6
#endif
#pragma poison
