// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }

export template <typename T, typename U>
struct same
{
  enum { value = 0 };
};

template<typename T>
struct same <T, T>
{
  enum { value = 1 };
};

