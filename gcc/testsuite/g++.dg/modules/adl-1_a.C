// { dg-additional-options -fmodules-ts }
export module worker;
// { dg-module-bmi worker }

namespace details {

export int fn (int x)
{
  return x;
}

}
