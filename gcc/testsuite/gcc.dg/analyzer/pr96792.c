#define NULL (void *)0

struct block
{
  void *function;
  const struct block *superblock;
};

struct global_block
{
  struct block block;
  void *compunit_symtab;
};

extern const struct block *block_global_block (const struct block *block);

void *
block_objfile (const struct block *block)
{
  const struct global_block *global_block;

  if (block->function != NULL)
    return block->function;

  global_block = (struct global_block *) block_global_block (block);
  return global_block->compunit_symtab;
}

const struct block *
block_global_block (const struct block *block)
{
  if (block == NULL)
    return NULL;

  while (block->superblock != NULL)
    block = block->superblock;

  return block;
}
