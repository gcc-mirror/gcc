// PR c++/13478

struct A {};
struct B : protected A {
    B() {};
    B(const A& ) {};
private:
    B(const B& ) {};
};

void foo(const A* ap)
{
  const B& br = *ap;
}
