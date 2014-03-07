// { dg-do compile { target c++11 } }

struct A { int i; };

void f (const A &);
void f (A &&);

void g (A, int);
void g (A, double);

int main()
{
  f ( { 1 } );
  g ( { 1 }, 1 );
}
