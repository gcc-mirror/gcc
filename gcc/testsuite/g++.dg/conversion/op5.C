// Contributed by Dodji Seketeli <dseketel@redhat.com>
// Origin: PR c++/36407 
// { dg-do compile }

struct A
{
  A (const A&);
};

struct B
{
  operator A&();
};

void
foo (const B& b)
{
  const A a = b; // { dg-error "conversion from 'const B' to non-scalar type 'const A' requested" }
}

