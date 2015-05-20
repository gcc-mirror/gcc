// 5.3.4/19: If the lookup finds the two-parameter form of a usual
// deallocation function (3.7.4.2) and that function, considered as a
// placement deallocation function, would have been selected as a match for
// the allocation function, the program is ill-formed.

// But we should only complain about using op delete (void *, size_t) for
// placement delete if it would also be selected for normal delete, not if
// there's also an op delete (void *).

typedef __SIZE_TYPE__ size_t;

struct A
{
  A();
  void* operator new (size_t, size_t);
  void operator delete (void *, size_t); // { dg-message "non-placement" }
};

struct B
{
  B();
  void * operator new (size_t);
  void * operator new (size_t, size_t);
  void operator delete (void *);
  void operator delete (void *, size_t);
};

int main()
{
  A* ap = new (24) A;		// { dg-error "placement" }
  B* bp = new (24) B;
}
