// { dg-do assemble  }
// GROUPS passed ambiguity
struct A {
  A (int);
};

struct B {
  B (int);
};

void myfunc (const A& t0); // { dg-error "" } 
void myfunc (const B& t0); // { dg-error "" } 

int main ()
{
   myfunc(1);   // { dg-error "" } ambiguous call
}
