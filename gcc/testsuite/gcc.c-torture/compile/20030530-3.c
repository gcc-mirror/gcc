struct tree_decl
{
  unsigned in_system_header_flag:1;
};
union tree_node
{
  struct tree_decl decl;
};
typedef union tree_node *tree;
static int
redeclaration_error_message (olddecl)
     tree olddecl;
{
  if (({olddecl;})->decl.in_system_header_flag)
    ;
}
