// PR c++/18161

namespace m
{
  namespace n
  {
  }
}

namespace n
{
}

namespace o
{
  namespace n = ::m::n;
}
