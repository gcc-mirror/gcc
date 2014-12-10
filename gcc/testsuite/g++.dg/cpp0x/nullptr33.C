// PR c++/63757
// { dg-do compile { target c++11 } }

typedef decltype(nullptr) nullptr_t;

void bar(void*) {}
 
struct foo
{
  operator nullptr_t()
  {
    return nullptr;
  }
};

int main()
{
  bar(foo());
}
