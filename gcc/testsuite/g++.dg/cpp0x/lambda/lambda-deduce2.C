// PR c++/43875
// { dg-options "-std=c++0x" }

void f();
void f(int);

int main()
{
  auto x1 = []{ return f; };	    // { dg-error "return|overloaded" }
  auto x2 = []{ return { 1, 2 }; }; // { dg-error "return|list" }
}
