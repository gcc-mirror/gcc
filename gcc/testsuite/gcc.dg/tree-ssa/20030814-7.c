/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */

extern void abort (void);
struct rtx_def;
typedef struct rtx_def *rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
union tree_node;
typedef union tree_node *tree;
struct tree_common
{
  int code;
};
union tree_node
{
  struct tree_common common;
};
extern tree current_function_decl;
struct cgraph_rtl_info
{
  _Bool pure_function;
};
struct cgraph_rtl_info *cgraph_rtl_info (tree);
void
mark_constant_function (void)
{
  rtx insn;
  int nonlocal_memory_referenced;

  if (current_function_decl->common.code != 42)
    abort ();

  cgraph_rtl_info (current_function_decl)->pure_function = 1;
}

/* current_function_decl should be loaded once into a temporary
   and the temporary used as the argument to cgraph_rtl_info.
   This if we find current_function_decl used as an argument, then
   we have failed.  */
/* { dg-final { scan-tree-dump-times "\\(current_function_decl\\)" 0 "dom2"} } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
