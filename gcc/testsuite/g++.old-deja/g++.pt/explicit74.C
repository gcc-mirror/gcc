// Reduced from a testcase by Yotam Medini <yotam@avanticorp.com>

// egcs 1.1 seems to generate code that deletes a NULL pointer.

template <class bar> struct foo { void fuz(); ~foo(); };
struct baz { int i; foo<baz> j; } *p = 0;
template <class bar> void foo<bar>::fuz() { delete p; }
template <class bar> foo<bar>::~foo() { delete p; }
template class foo<baz>;
int main() { foo<baz>(); }
