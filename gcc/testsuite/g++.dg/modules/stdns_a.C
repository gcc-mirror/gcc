// { dg-additional-options -fmodules-ts }

export module std;
// { dg-module-bmi std }

namespace std
{
export int frob () 
{
  return 1;
}

}
