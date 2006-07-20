// PR c++/28409
// shouldIbevisible should be emitted because it's an extern "C" decl with
// external linkage, even though it's in the anonymous namespace.

namespace
{
  extern "C" int shouldIbevisible()
  {
    return 0;
  }
}

namespace t
{
  extern "C" int shouldIbevisible(void);
}

int main(void)
{
  return t::shouldIbevisible();
}
