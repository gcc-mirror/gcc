/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */

extern void abort (void);
union tree_node;
typedef union tree_node *tree;


enum tree_code
{
  SET_TYPE,
  RECORD_TYPE,
  LAST_AND_UNUSED_TREE_CODE
};
extern const char tree_code_type[];

struct tree_common
{

  enum tree_code code:8;
};





union tree_node
{
  struct tree_common common;
};

readonly_fields_p (type)
     tree type;
{

  if (type->common.code != RECORD_TYPE)
    return;

  if (tree_code_type[type->common.code] != 't')
    abort ();

  return;
}

/* A good optimizer would realize that the cast to (unsigned int) is
   useless as the earlier cast of the same value of (unsigned char) will
   always produce the same result.  */
/* { dg-final { scan-tree-dump-times "\\(unsigned int\\)" 0 "dom3"} } */
 
/* There should be one load of ->common.code.  We currently fail this
   because we load from ->common.code using different types.  */
/* { dg-final { scan-tree-dump-times "common\.code" 1 "dom3"} } */
