/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */
                                                                                
struct die_struct;
typedef struct die_struct *dw_die_ref;
typedef struct dw_loc_list_struct *dw_loc_list_ref;
enum dw_val_class
{
  dw_val_class_loc_list,
};
typedef struct dw_val_struct
{
  enum dw_val_class val_class;
  union dw_val_struct_union
    {
      dw_loc_list_ref val_loc_list;
    }
  v;
}
dw_val_node;
typedef struct dw_attr_struct *dw_attr_ref;
typedef struct dw_attr_struct
{
  dw_val_node dw_attr_val;
}
dw_attr_node;

extern __inline__ enum dw_val_class
AT_class (a)
     dw_attr_ref a;
{
  return a->dw_attr_val.val_class;
}
extern __inline__ dw_loc_list_ref
AT_loc_list (a)
     dw_attr_ref a;
{
  if (AT_class (a) == dw_val_class_loc_list)
    return a->dw_attr_val.v.val_loc_list;
}
static void
output_location_lists (die)
     dw_die_ref die;
{
  dw_die_ref c;
  dw_attr_ref d_attr;
    if (AT_class (d_attr) == dw_val_class_loc_list)
      output_loc_list (AT_loc_list (d_attr));
}

/* There should be exactly one IF conditional, in output_location_lists.  */
/* { dg-final { scan-tree-dump-times "if " 1 "dom3"} } */
/* { dg-final { cleanup-tree-dump "dom3" } } */
