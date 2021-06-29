// { dg-additional-options "-fmodules-ts -fcontracts" }

module foo;

void foo (int x, void *p)
  [[ pre: x > 0 ]]
{
  auto *obj = reinterpret_cast<TPL<int> *> (p);

  obj->member = x;
}

void foo (float x, void *p)
  [[ pre: x > 0 ]]
{
  auto *obj = reinterpret_cast<TPL<float> *> (p);

  obj->member = x;
}
