#include "rust-compile-asm.h"
#include "rust-compile-expr.h"
namespace Rust {
namespace Compile {

CompileAsm::CompileAsm (Context *ctx) : HIRCompileBase (ctx) {}

tree
CompileAsm::tree_codegen_asm (HIR::InlineAsm &expr)
{
  auto asm_expr
    = asm_build_stmt (expr.get_locus (), {asm_construct_string_tree (expr),
					  asm_construct_outputs (expr),
					  asm_construct_inputs (expr),
					  asm_construct_clobber_tree (expr),
					  asm_construct_label_tree (expr)});

  ASM_BASIC_P (asm_expr) = expr.is_simple_asm ();
  ASM_VOLATILE_P (asm_expr) = false;
  ASM_INLINE_P (asm_expr) = expr.is_inline_asm ();
  /*Backend::debug (asm_expr);*/
  return asm_expr;
}

tree
CompileAsm::asm_build_stmt (
  location_t loc,
  const std::array<tree, CompileAsm::ASM_TREE_ARRAY_LENGTH> &trees)
{
  // Prototype functiion for building an ASM_EXPR tree.
  tree ret;
  bool side_effects;

  ret = make_node (ASM_EXPR);
  TREE_TYPE (ret) = void_type_node;
  SET_EXPR_LOCATION (ret, loc);

  /* TREE_SIDE_EFFECTS will already be set for statements with
     implicit side effects.  Here we make sure it is set for other
     expressions by checking whether the parameters have side
     effects.  */

  // This is here because of c-typeck.cc's code
  // I'm not sure what kind of effects it has
  side_effects = false;
  for (size_t i = 0; i < trees.size (); i++)
    {
      tree t = trees[i];
      if (t && !TYPE_P (t))
	side_effects |= TREE_SIDE_EFFECTS (t);
      TREE_OPERAND (ret, i) = t;
    }

  TREE_SIDE_EFFECTS (ret) |= side_effects;

  return ret;
}

tree
CompileAsm::asm_construct_string_tree (HIR::InlineAsm &expr)
{
  // To construct an ASM_EXPR tree, we need to build a STRING_CST tree.
  //
  // We do this by concatenating all the template strings in the InlineAsm
  // into one big std::string seperated by tabs and newlines. (For easier
  // debugging and reading)
  std::stringstream ss;
  for (const auto &template_str : expr.template_strs)
    ss << template_str.symbol << "\n";

  std::string result = ss.str ();
  return Backend::string_constant_expression (result);
}

tree
CompileAsm::asm_construct_outputs (HIR::InlineAsm &expr)
{
  // TODO: Do i need to do this?

  tree head = NULL_TREE;
  for (auto &output : expr.get_operands ())
    {
      if (output.get_register_type ()
	  == AST::InlineAsmOperand::RegisterType::Out)
	{
	  auto out = output.get_out ();

	  tree out_tree = CompileExpr::Compile (*out.expr, this->ctx);
	  // expects a tree list
	  // TODO: This assumes that the output is a register
	  std::string expr_name = "=r";
	  auto name = build_string (expr_name.size () + 1, expr_name.c_str ());
	  head
	    = chainon (head, build_tree_list (build_tree_list (NULL_TREE, name),
					      out_tree));

	  /*Backend::debug (head);*/
	  /*head = chainon (head, out_tree);*/
	}
    }
  return head;
}

tree
CompileAsm::asm_construct_inputs (HIR::InlineAsm &expr)
{
  // TODO: Do i need to do this?
  tree head = NULL_TREE;
  for (auto &input : expr.get_operands ())
    {
      if (input.get_register_type () == AST::InlineAsmOperand::RegisterType::In)
	{
	  auto in = input.get_in ();

	  tree in_tree = CompileExpr::Compile (*in.expr, this->ctx);
	  // expects a tree list
	  // TODO: This assumes that the input is a register
	  std::string expr_name = "r";
	  auto name = build_string (expr_name.size () + 1, expr_name.c_str ());
	  head
	    = chainon (head, build_tree_list (build_tree_list (NULL_TREE, name),
					      in_tree));

	  /*head = chainon (head, out_tree);*/
	}
    }
  return head;
}

tree
CompileAsm::asm_construct_clobber_tree (HIR::InlineAsm &expr)
{
  // TODO: Do i need to do this?
  return NULL_TREE;
}

tree
CompileAsm::asm_construct_label_tree (HIR::InlineAsm &expr)
{
  // TODO: Do i need to do this?
  return NULL_TREE;
}

} // namespace Compile
} // namespace Rust
