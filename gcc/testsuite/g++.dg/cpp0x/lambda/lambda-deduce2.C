// PR c++/43875
// { dg-do compile { target c++11 } }

void f();
void f(int);

int main()
{
  auto x1 = []{ return f; };	    // { dg-error "return|overloaded" }
  auto x2 = []{ return { 1, 2 }; }; // { dg-error "return|list" }
}
