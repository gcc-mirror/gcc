// PR c++/6179

// Bug: we tried to look at the fields of 'const A' to determine the proper
// exception specification for the synthesized copy constructor, but
// TYPE_FIELDS hadn't been set yet, so we incorrectly got a throw() spec.

struct B
{
  B () {}
  B (const B&) { throw 1; }
};

struct A;
void f (const A &) {}
struct A
{
  B b;
};

int main ()
{
  A a;  
  try
    { A a2 (a); }
  catch (...)
    { }
}
