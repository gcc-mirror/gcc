#pragma once

#include "rust-system.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Analysis {

template <class T> class Scope
{
public:
  Scope () : scopeStack () {}

  ~Scope () {}

  bool Insert (std::string key, T val)
  {
    if (scopeStack.back ().find (key) != scopeStack.back ().end ())
      {
	return false;
      }

    scopeStack.back ().insert (std::make_pair (key, std::move (val)));
    return true;
  }

  bool Lookup (std::string key, T *result)
  {
    for (auto it = scopeStack.rbegin (); it != scopeStack.rend (); ++it)
      {
	auto lookup = it->find (key);
	if (lookup != it->end ())
	  {
	    *result = lookup->second;
	    return true;
	  }
      }
    return false;
  }

  void Push () { scopeStack.push_back ({}); }

  std ::map<std::string, T> Pop ()
  {
    auto toplevel = scopeStack.back ();
    scopeStack.pop_back ();
    return toplevel;
  }

  std ::map<std::string, T> Peek () { return scopeStack.back (); }

private:
  std::vector<std::map<std::string, T> > scopeStack;
};

} // namespace Analysis
} // namespace Rust
