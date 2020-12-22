// { dg-additional-options -fmodules-ts }
export module worker;
// { dg-module-cmi worker }

namespace details {

int fn (int x)
{
  return x;
}

}
