struct tree_common
{
  int uid;
  unsigned int code : 8;
  unsigned int code2 : 8;
  unsigned external_attr : 1;
  unsigned public_attr : 1;

};

static int
duplicate_decls (x)
     register struct tree_common *x;
{
  if (x->external_attr)
    if (x->code)
      if (x->code2)
	x->public_attr = 1;
}
