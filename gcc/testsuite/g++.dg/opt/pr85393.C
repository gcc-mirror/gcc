// PR rtl-optimization/85393
// { dg-do run { target c++11 } }
// { dg-options "-O2" }
// { dg-additional-sources "pr85393-aux.cc" }
// { dg-skip-if "requires hosted libstdc++ for stdexcept" { ! hostedlib } }

#include <stdexcept>
#include <vector>

void foo (char const *s);
struct S { ~S () noexcept (false) { throw std::runtime_error ("foo"); } };

int
main (int argc, char *argv[])
{
  std::vector <std::vector <char> > args;
  try
    {
      {
        S k;
        foo ("A");
      }

      if (argv)
        throw std::runtime_error ("foo");
      args.push_back ({});
    }
  catch (std::runtime_error const& e)
    {}
}
