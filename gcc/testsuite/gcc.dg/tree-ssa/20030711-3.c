/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
  

struct rtx_def;
typedef struct rtx_def *rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
union tree_node;
typedef union tree_node *tree;
typedef struct mem_attrs
{
  long alias;
}
mem_attrs;
union rtunion_def
{
  mem_attrs *rtmem;
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  int code;
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
long
get_alias_set (t)
     tree t;
{
  if (t->decl.rtl != (void *) 0)
    return (((t->decl.rtl->fld[1]).rtmem) ==
            0 ? 0
            : ((((t->decl.
                  rtl ? 0 : (make_decl_rtl (t, ((void *) 0)),
                             t->decl.rtl)))->fld[1]).rtmem)->alias);
}

/* The calls to make_decl_rtl should be eliminated.  */
/* { dg-final { scan-tree-dump-not "make_decl_rtl \\(\\)" "dom2" } } */
    
/* There should be two IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 2 "dom2"} } */
                                                                                
/* There should be one load of decl.rtl.  */
/* { dg-final { scan-tree-dump-times "decl\\.rtl" 1 "dom2"} } */
  
/* There should be two loads of rtmem.  */
/* { dg-final { scan-tree-dump-times "rtmem" 2 "dom2"} } */

/* There should be one load of alias.  */
/* { dg-final { scan-tree-dump-times "->alias" 1 "dom2"} } */

/* { dg-final { cleanup-tree-dump "dom2" } } */
