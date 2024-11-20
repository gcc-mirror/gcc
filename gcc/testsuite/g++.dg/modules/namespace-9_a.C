// { dg-additional-options -fmodules }

export module M;

namespace A
{
  export using AT = int;
}

using namespace A;
inline AT var;
