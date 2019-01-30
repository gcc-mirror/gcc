// { dg-additional-options -fmodules-ts }
export module worker;
// { dg-module-bmi worker }

namespace details {

int fn (int x)
{
  return x;
}

}
