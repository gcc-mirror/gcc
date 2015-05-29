/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2 -fdelete-null-pointer-checks" } */
     
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

/* There should be no IF conditionals, unless target disables -fdelete-null-pointer-checks  */
/* { dg-final { scan-tree-dump-times "if " 0 "dom2" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump "if " "dom2" { target { keeps_null_pointer_checks } } } } */
     
