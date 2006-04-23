// Check that we handle bitfields as complex lvalues properly.

struct A
{
  int i: 2;
  int j: 2;
  int k: 2;
};

A a, a2;
bool b;
void f ();

int main ()
{
  (f(), a.j) = 1;
  (f(), a).j = 2; // { dg-warning "overflow" } 
  (b ? a.j : a2.k) = 3; // { dg-warning "overflow" } 
  (b ? a : a2).j = 0;
  ++(a.j) = 1;
  (a.j = 2) = 3; // { dg-warning "overflow" } 
}

    
