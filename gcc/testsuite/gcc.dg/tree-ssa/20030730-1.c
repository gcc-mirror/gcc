/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom3" } */
     
extern void exit (int);
extern void *ggc_alloc (__SIZE_TYPE__);
typedef struct dw_attr_struct *dw_attr_ref;
typedef struct dw_attr_struct
{
  int dw_attr;
}
dw_attr_node;
void
foo (int attr_kind, unsigned long offset)
{
  dw_attr_ref attr = (dw_attr_ref) ggc_alloc (sizeof (dw_attr_node));
  attr->dw_attr = attr_kind;
  if (attr != 0)
    exit (0);
}

/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dom3"} } */
                                                                                
/* { dg-final { cleanup-tree-dump "dom3" } } */
