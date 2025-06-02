// { dg-additional-options "-fmodules" }

export module M;

export namespace B
{
  int i;
}
export namespace C
{
  using namespace B;
}
