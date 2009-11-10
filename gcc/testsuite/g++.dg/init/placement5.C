// 5.3.4/19: If the lookup finds the two-parameter form of a usual
// deallocation function (3.7.4.2) and that function, considered as a
// placement deallocation function, would have been selected as a match for
// the allocation function, the program is ill-formed.

typedef __SIZE_TYPE__ size_t;

struct A
{
  A();
  static void* operator new (size_t, size_t);
  static void operator delete (void *, size_t); // { dg-error "non-placement" }
};

int main()
{
  A* ap = new (24) A;		// { dg-error "placement delete" }
}
