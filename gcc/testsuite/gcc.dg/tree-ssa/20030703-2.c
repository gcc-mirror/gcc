/* { dg-do compile } */
/* { dg-options "-Wno-old-style-definition -O1 -fdump-tree-dom2" } */

extern void abort (void);

union tree_node;
typedef union tree_node *tree;
extern const char tree_code_type[];

union tree_node
{
  int code;
  long pointer_alias_set;
};

long
get_alias_set (t)
     tree t;
{
  if (tree_code_type[t->code])
    abort ();
  if (t->pointer_alias_set)
    {
      tree __t = t;
      if (tree_code_type[__t->code])
	abort ();
    }
}

/* There should be precisely one load of {t,__t}->code.  If there is
   more than one, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "->code" 1 "dom2"} } */
                                                                                
/* There should be precisely one load of tree_code_type.  If there is
   more than one, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "tree_code_type" 1 "dom2"} } */

/* There should be one IF conditional.  If 'tree_code_type[t->code]' is
   zero, then the third if() conditional is unnecessary.  That should cause
   the call to abort() to be removed, which in turn causes the whole second
   if() to disappear.  */
/* { dg-final { scan-tree-dump-times "if " 1 "dom2"} } */

