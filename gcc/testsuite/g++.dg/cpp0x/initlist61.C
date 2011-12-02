// { dg-do compile { target c++11 } }

struct N { N(int); };
struct A { N i,j; };

int main()
{
  A* ap = new A{1,2};
}
