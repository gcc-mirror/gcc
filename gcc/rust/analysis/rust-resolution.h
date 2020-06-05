#pragma once

#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-scan.h"
#include "scope.h"

namespace Rust {
namespace Analysis {

class Resolution : public AST::ASTVisitor
{
public:
  virtual ~Resolution ()
  {
    scope.Pop ();
    valueScope.Pop ();
    macroScope.Pop ();
    typeScope.Pop ();
  };

private:
  virtual bool go () = 0;

protected:
  Resolution (AST::Crate &crate, TopLevelScan &toplevel)
    : crate (crate), toplevel (toplevel)
  {
    scope.Push ();
    valueScope.Push ();
    macroScope.Push ();
    typeScope.Push ();
  };

  Scope<AST::Type *> scope;
  Scope<AST::Type *> valueScope;
  Scope<AST::Type *> macroScope;
  Scope<AST::Type *> typeScope;

  AST::Crate &crate;
  TopLevelScan &toplevel;

  std::vector<AST::IdentifierPattern> letPatternBuffer;
  std::vector<AST::Type *> typeBuffer;
  std::vector<std::string> typeComparisonBuffer;
  std::vector<AST::Function *> functionLookup;
};

} // namespace Analysis
} // namespace Rust
