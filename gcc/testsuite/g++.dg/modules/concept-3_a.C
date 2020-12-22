// { dg-additional-options "-fmodules-ts -std=c++2a" }

export module foo;
// { dg-module-cmi foo }

namespace foo
{
export template<typename _Tp>
concept Addable = requires(_Tp& __t)
  {
    __t + __t;
  };
}
