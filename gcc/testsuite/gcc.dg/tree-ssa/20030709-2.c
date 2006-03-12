/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce" } */
  
struct rtx_def;
typedef struct rtx_def *rtx;
union tree_node;
typedef union tree_node *tree;
typedef struct mem_attrs
{
  int  foo;

} mem_attrs;
union rtunion_def
{
  mem_attrs *rtmem;
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  rtunion fld[1];
};
struct tree_decl
{
  rtx rtl;
};
union tree_node
{
  struct tree_decl decl;
};
void *
get_alias_set (t)
     tree t;
{
  long set;
  if (t->decl.rtl)
    return (t->decl.rtl->fld[1].rtmem 
	    ? 0
	    : (((t->decl.rtl ? t->decl.rtl: (make_decl_rtl (t, 0), t->decl.rtl)))->fld[1]).rtmem);
  return (void*)-1;
}

/* There should be precisely one load of ->decl.rtl.  If there is
   more than, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "->decl\\.rtl" 1 "cddce"} } */
  
/* There should be no loads of .rtmem since the complex return statement
   is just "return 0".  */
/* { dg-final { scan-tree-dump-times ".rtmem" 0 "cddce"} } */
  
/* There should be one IF statement (the complex return statement should
   collapse down to a simple return 0 without any conditionals).  */
/* { dg-final { scan-tree-dump-times "if " 1 "cddce"} } */

/* { dg-final { cleanup-tree-dump "cddce" } } */
