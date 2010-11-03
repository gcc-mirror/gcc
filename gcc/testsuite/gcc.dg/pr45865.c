/* PR rtl-optimization/45865 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

typedef union tree_node *tree;
enum ix86_builtin_type {
  IX86_BT_LAST_VECT,
  IX86_BT_LAST_PTR
};
extern const enum ix86_builtin_type ix86_builtin_type_ptr_base[];
extern tree build_qualified_type (tree, int);
extern tree build_pointer_type (tree);
tree
ix86_get_builtin_type (enum ix86_builtin_type tcode, unsigned int index)
{
  tree type, itype;
  int quals;
  if (tcode <= IX86_BT_LAST_PTR)
    quals = 0x0;
  else
    quals = 0x1;
  itype = ix86_get_builtin_type (ix86_builtin_type_ptr_base[index],
				 index);
  if (quals != 0x0)
    itype = build_qualified_type (itype, quals);
  type = build_pointer_type (itype);
  return type;
}
