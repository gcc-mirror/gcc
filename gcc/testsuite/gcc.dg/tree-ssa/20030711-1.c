/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
 
extern void abort (void);
extern void blah (void);

union tree_node;
typedef union tree_node *tree;
struct tree_vec
{
          int length;
            tree a[1];
};
struct tree_type
{
          tree binfo;
};
union tree_node
{
          struct tree_type type;
            struct tree_vec vec;
};

void
record_component_aliases (type)
     tree type;
{
  int i;
  if (4 >= type->type.binfo->vec.length)
    abort ();
  for (; i < ((
                {
                const tree __t = type->type.binfo;
                if (4 >= __t->vec.length)
                abort (); type->type.binfo->vec.a[4];}
              )->vec.length);)
    {
      if (4 >= type->type.binfo->vec.length)
        abort ();
      blah ();
    }
}

/* The call to blah can not be eliminated.  */
/* { dg-final { scan-tree-dump-times "blah \\(\\)" 1 "dom2" } } */
   
/* There should be three IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 3 "dom2"} } */
                                                                                
/* There should be two loads of type.binfo.  */
/* { dg-final { scan-tree-dump-times "type\\.binfo" 2 "dom2"} } */
 
/* There should be three loads of vec.length.  */
/* { dg-final { scan-tree-dump-times "vec.length" 3 "dom2"} } */

