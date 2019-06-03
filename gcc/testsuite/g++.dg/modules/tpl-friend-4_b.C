// { dg-additional-options -fmodules-ts }

module foo;

template class TPL<int>;
template class DEF<int>;

void m ()
{
  // ADL to find hidden functions
  foo (1, (TPL<int> *)0);
  foo (1.0f, (TPL<int> *)0);

  // no ADL, no find
  {
    foo (1, 0);  // { dg-error "not declared" }
  }
  {
    foo (1.0f, 0); // { dg-error "not declared" }
  }
}
