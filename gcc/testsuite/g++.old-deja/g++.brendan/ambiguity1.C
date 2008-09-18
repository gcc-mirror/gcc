// { dg-do assemble  }
// GROUPS passed ambiguity
struct A {
  A (int);
};

struct B {
  B (int);
};

void myfunc (const A& t0); // { dg-message "candidates" } 
void myfunc (const B& t0); // { dg-message "note" } 

int main ()
{
   myfunc(1);   // { dg-error "ambiguous" }
}
