#include "rust-compile-asm.h"

#include "rust-tree.h"
#include "rust-system.h"
namespace Rust {
namespace Compile {

tree
CompileAsm::add_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (EXPR_P (t) && code != LABEL_EXPR)
    {
      if (!EXPR_HAS_LOCATION (t))
	SET_EXPR_LOCATION (t, input_location);

      /* When we expand a statement-tree, we must know whether or not the
	 statements are full-expressions.  We record that fact here.  */
      if (STATEMENT_CODE_P (TREE_CODE (t)))
	STMT_IS_FULL_EXPR_P (t) = stmts_are_full_exprs_p ();
    }

  if (code == LABEL_EXPR || code == CASE_LABEL_EXPR)
    STATEMENT_LIST_HAS_LABEL (cur_stmt_list) = 1;

  /* Add T to the statement-tree.  Non-side-effect statements need to be
     recorded during statement expressions.  */
  gcc_checking_assert (!stmt_list_stack->is_empty ());
  append_to_statement_list_force (t, &cur_stmt_list);

  return t;
}
tree
CompileAsm::asm_build_asm_stmt (HIR::InlineAsm &expr)
{
  // From the implementation of c-typeck.cc
  // tree
  // build_asm_stmt (bool is_volatile, tree args)
  //{
  //   if (is_volatile)
  //     ASM_VOLATILE_P (args) = 1;
  //   return add_stmt (args);
  // }
  //
  return add_stmt (asm_build_expr (expr));
}
tree
CompileAsm::asm_build_expr (HIR::InlineAsm &expr)
{
  auto asm_expr = asm_build_stmt (asm_get_locus (expr), ASM_EXPR,
				  {asm_construct_string_tree (expr),
				   asm_construct_outputs (expr),
				   asm_construct_inputs (expr),
				   asm_construct_clobber_tree (expr),
				   asm_construct_label_tree (expr)});

  ASM_BASIC_P (asm_expr) = CompileAsm::asm_is_simple (expr);
  ASM_VOLATILE_P (asm_expr) = (false);
  ASM_INLINE_P (asm_expr) = CompileAsm::asm_is_inline (expr);
  return asm_expr;
  // return build_asm_expr (CompileAsm::asm_get_locus (expr),
  //			 CompileAsm::asm_construct_string_tree (expr),
  //			 CompileAsm::asm_construct_outputs (expr),
  //			 CompileAsm::asm_construct_inputs (expr),
  //			 CompileAsm::asm_construct_clobber_tree (expr),
  //			 CompileAsm::asm_construct_label_tree (expr),
  //			 CompileAsm::asm_is_simple (expr),
  //			 CompileAsm::asm_is_inline (expr));
}

tree
CompileAsm::asm_build_stmt (
  location_t loc, enum tree_code code,
  const std::array<tree, CompileAsm::ASM_TREE_ARRAY_LENGTH> &trees)
{
  tree ret;
  //  va_list p;
  bool side_effects;

  /* This function cannot be used to construct variably-sized nodes.  */
  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  // va_start (p, code);

  ret = make_node (code);
  TREE_TYPE (ret) = void_type_node;
  SET_EXPR_LOCATION (ret, loc);

  /* TREE_SIDE_EFFECTS will already be set for statements with
     implicit side effects.  Here we make sure it is set for other
     expressions by checking whether the parameters have side
     effects.  */

  side_effects = false;
  for (size_t i = 0; i < trees.size (); i++)
    {
      tree t = trees[i];
      if (t && !TYPE_P (t))
	side_effects |= TREE_SIDE_EFFECTS (t);
      TREE_OPERAND (ret, i) = t;
    }

  TREE_SIDE_EFFECTS (ret) |= side_effects;

  // va_end (p);
  return ret;
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

  string_chain = nreverse (string_chain);

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
