// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

export template <typename T>
struct TPL 
{
  typename T::type m;
  using type = typename T::type;
  const type cm;
};

