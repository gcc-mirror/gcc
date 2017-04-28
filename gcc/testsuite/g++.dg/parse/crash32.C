// { dg-do compile }
struct Visitor;

struct Ast
{
  virtual void accept (Visitor& v);
};

void
Ast::accept (Visitor& v)
{
  v (*this); // { dg-error "no match for call" }
}
