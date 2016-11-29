// { dg-options "-std=c++14 -Waligned-new" }

struct alignas(64) A { int i; };
struct alignas(64) B {
  int i;
  void *operator new(__SIZE_TYPE__);
};

int main()
{
  A* ap = new A;		// { dg-warning "-Waligned-new" }
  B* bp = new B;
}
