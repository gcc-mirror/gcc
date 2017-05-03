/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp1" } */

/* We should thread the if (!in_loop) completely leaving
   just two conditionals.  */
/* { dg-final { scan-tree-dump-times "if \\(" 2 "vrp1" } } */


union tree_node;
typedef union tree_node *tree;

enum size_type_kind
{
  SIZETYPE,
  SSIZETYPE,
  BITSIZETYPE,
  SBITSIZETYPE,
  TYPE_KIND_LAST
};
extern tree size_int_kind (long, enum size_type_kind);



typedef struct
{

  tree base, step;

} affine_iv;

struct loop
{

  int num;
};
extern unsigned char simple_iv ();

unsigned char
dr_analyze_innermost (struct loop *loop, tree poffset)
{
  affine_iv offset_iv;
  unsigned char in_loop = (loop && loop->num);


  if (in_loop)
    simple_iv ();

  if (!in_loop)
    offset_iv.step = size_int_kind (0, SSIZETYPE);

}
