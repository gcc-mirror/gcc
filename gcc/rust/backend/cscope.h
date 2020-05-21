#pragma once

#include "rust-system.h"
#include "rust-backend.h"
#include "scope.h"

namespace Rust {
namespace Compile {

class Scope
{
public:
  Scope (Backend *backend) : backend (backend) {}

  ~Scope () {}

  void Push ()
  {
    fndecls.Push ();
    vars.Push ();
    types.Push ();
  }

  void Pop ()
  {
    fndecls.Pop ();
    vars.Pop ();
    types.Pop ();
  }

  void PushCurrentFunction (std::string name, Bfunction *fn)
  {
    InsertFunction (name, fn);
    fns.push_back (fn);
  }

  Bfunction *PopCurrentFunction ()
  {
    auto ret = fns.back ();
    fns.pop_back ();
    return ret;
  }

  Bfunction *GetCurrentFndecl () { return fns.back (); }

  void PushBlock (Bblock *block)
  {
    blocks.push_back (block);
    std::vector<Bstatement *> empty;
    context.push_back (empty);
  }

  Bblock *PopBlock ()
  {
    auto ret = blocks.back ();
    blocks.pop_back ();

    auto stmts = context.back ();
    context.pop_back ();

    backend->block_add_statements (ret, stmts);

    return ret;
  }

  void AddStatement (Bstatement *stmt) { context.back ().push_back (stmt); }

  void InsertFunction (std::string name, Bfunction *fn)
  {
    fndecls.Insert (name, fn);
  }

  bool LookupFunction (std::string name, Bfunction **fn)
  {
    return fndecls.Lookup (name, fn);
  }

  void InsertType (std::string name, Btype *type) { types.Insert (name, type); }

  bool LookupType (std::string name, Btype **type)
  {
    return types.Lookup (name, type);
  }

  void InsertVar (std::string name, Bvariable *var) { vars.Insert (name, var); }

  bool LookupVar (std::string name, Bvariable **var)
  {
    return vars.Lookup (name, var);
  }

private:
  Backend *backend;

  ::std::vector<Bfunction *> fns;
  ::std::vector<Bblock *> blocks;
  ::std::vector< ::std::vector<Bstatement *> > context;

  Analysis::Scope<Bfunction *> fndecls;
  Analysis::Scope<Bvariable *> vars;
  Analysis::Scope<Btype *> types;
};

} // namespace Compile
} // namespace Rust
