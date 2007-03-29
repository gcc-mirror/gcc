// { dg-options "-pedantic" }

class foo
{
  foo() {};      // { dg-error "extra" }
};
