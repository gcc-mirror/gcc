// { dg-additional-options -fmodules }

export module M1;

namespace A
{
  export using AT = int;
}

export using namespace A;
inline AT var;
