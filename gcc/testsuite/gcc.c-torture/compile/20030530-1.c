union tree_node;
typedef union tree_node *tree;
struct tree_common
{
  tree type;
  unsigned lang_flag_0 : 1;
};
union tree_node
{
  struct tree_common common;
};
void bar (tree);
static void
java_check_regular_methods (tree class_decl)
{
  int saw_constructor = class_decl->common.type->common.lang_flag_0;
  tree class = class_decl->common.type;
  for (;;)
    {
      if (class)
        if (class_decl->common.type)
          bar (class);
    }
}
