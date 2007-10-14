/* { dg-do compile } */
// PR C++/30303
// This used to ICE because we did not return NULL
// in grokfndecl when an error happened.

struct Ifoo
{
virtual ~Ifoo(){}
};
struct foo : Ifoo
{
 foo(){};
};
foo::~foo() // { dg-error "definition of implicitly-declared" }
{
delete this;
}
