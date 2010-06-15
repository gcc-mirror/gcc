// { dg-do assemble  }

template <class T>
void f(T) {}			// { dg-error "initializing" }

class C;    // { dg-error "forward declaration" }

void g(const C& c)
{
  f(c); // { dg-error "invalid use of incomplete type" }
}
