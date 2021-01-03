// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

class bad_optional_access
{
public:

  virtual ~bad_optional_access() noexcept = default; // { dg-bogus "" }
};

inline void
__throw_bad_optional_access()
{
  throw bad_optional_access ();
}

