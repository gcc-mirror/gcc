struct block;

static int
remove_out_of_scope_renamings (struct block *current_block)
{
  return 1;
}
int
ada_lookup_symbol_list (const struct block *block0)
{
  return remove_out_of_scope_renamings ((struct block *) block0);
}
