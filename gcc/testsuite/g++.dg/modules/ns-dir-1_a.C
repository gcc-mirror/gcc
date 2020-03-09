// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }

namespace detail {
int i;
}

namespace elsewhere {

export inline void frob ()
{
  using namespace detail;
  i = 5;
}

}
