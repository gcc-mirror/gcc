// { dg-additional-options -fmodules-ts }

module foo;

template class TPL<int>;
// (instantiated in interface) template class TPL<float>;

void m ()
{
  // friend definitions instantiated on-demand here
  foo (1, 0);
  foo (1.0f, 0);
}

