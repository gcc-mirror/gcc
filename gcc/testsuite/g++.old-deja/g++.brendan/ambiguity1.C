// Build don't link: 
// GROUPS passed ambiguity
struct A {
  A (int);
};

struct B {
  B (int);
};

void myfunc (const A& t0); // ERROR - 
void myfunc (const B& t0); // ERROR - 

int main ()
{
   myfunc(1);   // ERROR - ambiguous call
}
