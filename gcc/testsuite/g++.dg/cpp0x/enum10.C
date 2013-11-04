// PR c++/48534
// { dg-options -std=c++11 }

enum class OpSE : bool;

int main()
{
  return static_cast<bool>(OpSE());
}
