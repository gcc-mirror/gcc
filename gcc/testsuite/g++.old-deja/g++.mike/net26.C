// A test case found by InterViews testing...

extern "C" int printf(const char *, ...);

class A {
public:
	int foo() { printf("ok nv\n"); return 0; }
	virtual int vfoo() { printf("ok v\n"); return 0; }
};

struct S {
	int (A::*pfn1)();
	int (A::*pfn2)();
	int (A::*pfn3)();
};

// This failed before.
S s = { &A::foo, &A::vfoo, &A::foo };

A a;

int main() {
  (a.*s.pfn1)();
  (a.*s.pfn2)();
  printf("PASS\n");
  return 0;
}
