// PR c++/71913
// { dg-do link { target c++11 } }

void* operator new(unsigned long, void* p) { return p; }

struct IndirectReturn {
  IndirectReturn() {}
  // Undefined so we get a link error if the indirect return value is copied
  IndirectReturn(const IndirectReturn&);
  IndirectReturn& operator=(const IndirectReturn&) = delete;
  ~IndirectReturn() {}
};

IndirectReturn foo() { return IndirectReturn(); }

void bar(void* ptr) {
  new (ptr) IndirectReturn(foo());
}

alignas (alignof (IndirectReturn))
unsigned char c[sizeof(IndirectReturn)];

int main()
{
  bar(c);
}

