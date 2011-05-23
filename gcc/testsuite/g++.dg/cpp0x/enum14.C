// PR c++/48106
// { dg-options -std=c++0x }

enum class E : char
{
  e
};

bool operator&(E e, char m)
{
  return static_cast<int>(e) & m;
}
