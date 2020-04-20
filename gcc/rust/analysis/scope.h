#pragma once

#include "rust-system.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Analysis {

class Scope
{
public:
  Scope () : scopeStack () {}

  ~Scope () {}

  bool Insert (std::string key, AST::Type *val)
  {
    if (scopeStack.back ().find (key) != scopeStack.back ().end ())
      {
	return false;
      }

    scopeStack.back ().insert (std::make_pair (key, std::move (val)));
    return true;
  }

  bool Lookup (std::string key, AST::Type **result)
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

  std ::map<std::string, AST::Type *> Pop ()
  {
    auto toplevel = scopeStack.back ();
    scopeStack.pop_back ();
    return toplevel;
  }

private:
  std::vector<std::map<std::string, AST::Type *> > scopeStack;
};

} // namespace Analysis
} // namespace Rust
