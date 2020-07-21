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
    structDecls.Push ();
  }

  void Pop ()
  {
    fndecls.Pop ();
    vars.Pop ();
    types.Pop ();
    structDecls.Pop ();
  }

  void PushCurrentFunction (std::string name, Bfunction *fn, Btype *retType,
			    Bvariable *retDecl)
  {
    fns.push_back (fn);
    fnRetType.push_back (retType);
    fnRetDecl.push_back (retDecl);
  }

  Bfunction *PopCurrentFunction ()
  {
    auto ret = fns.back ();
    fns.pop_back ();
    fnRetType.pop_back ();
    fnRetDecl.pop_back ();
    return ret;
  }

  Bfunction *GetCurrentFndecl () { return fns.back (); }

  Btype *GetCurrentFnRetType () { return fnRetType.back (); }

  Bvariable *GetCurrentFnRetDecl () { return fnRetDecl.back (); }

  Btype *GetFnRetType (Bfunction *fn)
  {
    auto it = fnRetTypeMapping.find (fn);
    if (it == fnRetTypeMapping.end ())
      {
	return NULL;
      }
    return it->second;
  }

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

  Bblock *CurBlock () { return blocks.back (); }

  void AddStatement (Bstatement *stmt) { context.back ().push_back (stmt); }

  void InsertStructDecl (std::string name, AST::StructStruct *decl)
  {
    structDecls.Insert (name, decl);
  }

  bool LookupStructDecl (std::string name, AST::StructStruct **decl)
  {
    return structDecls.Lookup (name, decl);
  }

  void InsertFunction (std::string name, Bfunction *fn, Btype *retType)
  {
    fndecls.Insert (name, fn);
    fnRetTypeMapping[fn] = retType;
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
  ::std::vector< ::Btype *> fnRetType;
  ::std::vector< ::Bvariable *> fnRetDecl;
  ::std::map<Bfunction *, Btype *> fnRetTypeMapping;

  Analysis::Scope<Bfunction *> fndecls;
  Analysis::Scope<Bvariable *> vars;
  Analysis::Scope<Btype *> types;
  Analysis::Scope<AST::StructStruct *> structDecls;
};

} // namespace Compile
} // namespace Rust
