// { dg-additional-options {-fmodules-ts -Wno-pedantic} }
module ;
       
# 4 "bug_a.ii" 1

namespace STD::RANGES::INNER
{
void Frob ();
}

struct gnu_char_traits
{
  void Frob()
  {
    STD::RANGES::INNER::Frob ();
  }
};

# 19 "" 2

export  module  hello;
// { dg-module-cmi hello }
export void greeter (gnu_char_traits const &name);
