#include "rust-compile-asm.h"

#include "rust-tree.h"
#include "rust-system.h"
namespace Rust {
namespace Compile {

tree
CompileAsm::asm_build_expr (HIR::InlineAsm &expr)
{
  return NULL_TREE;
  // return build_asm_expr (CompileAsm::asm_get_locus (expr),
  //			 CompileAsm::asm_construct_string_tree (expr),
  //			 CompileAsm::asm_construct_outputs (expr),
  //			 CompileAsm::asm_construct_inputs (expr),
  //			 CompileAsm::asm_construct_clobber_tree (expr),
  //			 CompileAsm::asm_construct_label_tree (expr),
  //			 CompileAsm::asm_is_simple (expr),
  //			 CompileAsm::asm_is_inline (expr));
}

location_t
CompileAsm::asm_get_locus (HIR::InlineAsm &expr)
{
  return expr.get_locus ();
}
tree
CompileAsm::asm_construct_string_tree (HIR::InlineAsm &expr)
{
  if (expr.template_strs.empty ())
    return build_string (1, "");
  // Initialize to NULL_TREE
  tree string_chain = NULL_TREE;

  for (const auto &template_str : expr.template_strs)
    {
      auto str = template_str.symbol;
      auto string_tree = build_string (str.size () + 1, str.c_str ());

      string_chain = tree_cons (NULL_TREE, string_tree, string_chain);
    }
  // Reverse the chain before returning
  return nreverse (string_chain);
}
tree
CompileAsm::asm_construct_outputs (HIR::InlineAsm &expr)
{
  return NULL_TREE;
}

tree
CompileAsm::asm_construct_inputs (HIR::InlineAsm &expr)
{
  return NULL_TREE;
}

tree
CompileAsm::asm_construct_clobber_tree (HIR::InlineAsm &expr)
{
  return NULL_TREE;
}
tree
CompileAsm::asm_construct_label_tree (HIR::InlineAsm &expr)
{
  return NULL_TREE;
}

bool
CompileAsm::asm_is_simple (HIR::InlineAsm &expr)
{
  return true;
}

bool
CompileAsm::asm_is_inline (HIR::InlineAsm &expr)
{
  return true;
}
} // namespace Compile
} // namespace Rust
