// { dg-additional-options -fmodules-ts }

export module std;
// { dg-module-cmi std }

namespace std
{
export int frob () 
{
  return 1;
}

}
