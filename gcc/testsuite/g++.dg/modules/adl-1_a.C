// { dg-module-do run }
// { dg-additional-options -fmodules-ts }
export module worker;
// { dg-module-cmi worker }

namespace details {

export int fn (int x)
{
  return x;
}

}
