#include "rust-compile-asm.h"

#include "rust-tree.h"
#include "rust-system.h"
#include <cstddef>
namespace Rust {
namespace Compile {

CompileAsm::CompileAsm (Context *ctx)
  : HIRCompileBase (ctx), translated (error_mark_node)
{}
void
CompileAsm::visit (HIR::InlineAsm &expr)
{
  return ctx->add_statement (asm_build_expr (expr));
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
  return NULL_TREE;
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
  else
    return build_string (4, "nop");
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
  //
  //
  // The following section serves as documentation for PR revieweres and future
  // asm developers. It documents the inspriation for the implementation of the
  // CompileAsm class

// From the implementation of c-typeck.cc
// tree
// build_asm_stmt (bool is_volatile, tree args)
//{
//   if (is_volatile)
//     ASM_VOLATILE_P (args) = 1;
//   return add_stmt (args);
// }
//
///* Build an asm-expr, whose components are a STRING, some OUTPUTS,
//   some INPUTS, and some CLOBBERS.  The latter three may be NULL.
//   SIMPLE indicates whether there was anything at all after the
//   string in the asm expression -- asm("blah") and asm("blah" : )
//   are subtly different.  We use a ASM_EXPR node to represent this.
//   LOC is the location of the asm, and IS_INLINE says whether this
//   is asm inline.  */
// tree
// build_asm_expr (location_t loc, tree string, tree outputs, tree inputs,
//		tree clobbers, tree labels, bool simple, bool is_inline)
//{
//  tree tail;
//  tree args;
//  int i;
//  const char *constraint;
//  const char **oconstraints;
//  bool allows_mem, allows_reg, is_inout;
//  int ninputs, noutputs;
//
//  ninputs = list_length (inputs);
//  noutputs = list_length (outputs);
//  oconstraints = (const char **) alloca (noutputs * sizeof (const char *));
//
//  string = resolve_asm_operand_names (string, outputs, inputs, labels);
//
//  /* Remove output conversions that change the type but not the mode.  */
//  for (i = 0, tail = outputs; tail; ++i, tail = TREE_CHAIN (tail))
//    {
//      tree output = TREE_VALUE (tail);
//
//      output = c_fully_fold (output, false, NULL, true);
//
//      /* ??? Really, this should not be here.  Users should be using a
//	 proper lvalue, dammit.  But there's a long history of using casts
//	 in the output operands.  In cases like longlong.h, this becomes a
//	 primitive form of typechecking -- if the cast can be removed, then
//	 the output operand had a type of the proper width; otherwise we'll
//	 get an error.  Gross, but ...  */
//      STRIP_NOPS (output);
//
//      if (!lvalue_or_else (loc, output, lv_asm))
//	output = error_mark_node;
//
//      if (output != error_mark_node
//	  && (TREE_READONLY (output)
//	      || TYPE_READONLY (TREE_TYPE (output))
//	      || (RECORD_OR_UNION_TYPE_P (TREE_TYPE (output))
//		  && C_TYPE_FIELDS_READONLY (TREE_TYPE (output)))))
//	readonly_error (loc, output, lv_asm);
//
//      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (tail)));
//      oconstraints[i] = constraint;
//
//      if (parse_output_constraint (&constraint, i, ninputs, noutputs,
//				   &allows_mem, &allows_reg, &is_inout))
//	{
//	  /* If the operand is going to end up in memory,
//	     mark it addressable.  */
//	  if (!allows_reg && !c_mark_addressable (output))
//	    output = error_mark_node;
//	  if (!(!allows_reg && allows_mem)
//	      && output != error_mark_node
//	      && VOID_TYPE_P (TREE_TYPE (output)))
//	    {
//	      error_at (loc, "invalid use of void expression");
//	      output = error_mark_node;
//	    }
//	}
//      else
//	output = error_mark_node;
//
//      TREE_VALUE (tail) = output;
//    }
//
//  for (i = 0, tail = inputs; tail; ++i, tail = TREE_CHAIN (tail))
//    {
//      tree input;
//
//      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (tail)));
//      input = TREE_VALUE (tail);
//
//      if (parse_input_constraint (&constraint, i, ninputs, noutputs, 0,
//				  oconstraints, &allows_mem, &allows_reg))
//	{
//	  /* If the operand is going to end up in memory,
//	     mark it addressable.  */
//	  if (!allows_reg && allows_mem)
//	    {
//	      input = c_fully_fold (input, false, NULL, true);
//
//	      /* Strip the nops as we allow this case.  FIXME, this really
//		 should be rejected or made deprecated.  */
//	      STRIP_NOPS (input);
//	      if (!c_mark_addressable (input))
//		input = error_mark_node;
//	    }
//	  else
//	    {
//	      struct c_expr expr;
//	      memset (&expr, 0, sizeof (expr));
//	      expr.value = input;
//	      expr = convert_lvalue_to_rvalue (loc, expr, true, false);
//	      input = c_fully_fold (expr.value, false, NULL);
//
//	      if (input != error_mark_node && VOID_TYPE_P (TREE_TYPE (input)))
//		{
//		  error_at (loc, "invalid use of void expression");
//		  input = error_mark_node;
//		}
//	    }
//	}
//      else
//	input = error_mark_node;
//
//      TREE_VALUE (tail) = input;
//    }
//
//  args = build_stmt (loc, ASM_EXPR, string, outputs, inputs, clobbers,
//  labels);
//
//  /* asm statements without outputs, including simple ones, are treated
//     as volatile.  */
//  ASM_INPUT_P (args) = simple;
//  ASM_VOLATILE_P (args) = (noutputs == 0);
//  ASM_INLINE_P (args) = is_inline;
//
//  return args;
//}
