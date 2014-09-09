struct tree_common
{
  unsigned int code : 9;
  unsigned int code2 : 7;
};

static int
duplicate_decls (x)
     register struct tree_common x;
{
  return x.code2;
}
