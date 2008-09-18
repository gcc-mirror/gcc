// { dg-do compile  }

template <class T = int>
struct A { // { dg-error "assignment" "assignment" }
// { dg-message "instantiated" "inst" { target *-*-* } 4 }
  const T x;
  A() : x(0) { } A(T x) : x(x) { }
}; 

template <class B>
void func ()
{
  B y; 
  y = B();  // { dg-message "synthesized" }
}

int main (void) { func< A<> >(); }
