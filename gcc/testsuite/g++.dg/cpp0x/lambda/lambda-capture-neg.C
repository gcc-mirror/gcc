// PR c++/50736
// { dg-do compile { target c++11 } }

int i;
void f();
typedef int T;

int main()
{
  [i]{};			// { dg-error "non-automatic" }
  [f]{};			// { dg-error "non-variable" }
  [T]{};			// { dg-error "non-variable" }
}

struct A { };
