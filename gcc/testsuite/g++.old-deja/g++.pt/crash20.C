// { dg-do compile  }

template <class T = int>
struct A { // { dg-message "const|operator=" "assignment" }
  const T x;
  A() : x(0) { } A(T x) : x(x) { }
}; 

template <class B>
void func ()
{
  B y; 
  y = B();  // { dg-message "synthesized|deleted" }
}

int main (void) { func< A<> >(); }
