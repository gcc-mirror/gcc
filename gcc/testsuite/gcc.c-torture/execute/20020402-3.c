/* extracted from gdb sources */

typedef unsigned long long CORE_ADDR;

struct blockvector;

struct symtab {
  struct blockvector *blockvector;
};

struct sec {
  void *unused;
};

struct symbol {
  int len;
  char *name;
};

struct block {
	CORE_ADDR startaddr, endaddr;
	struct symbol *function;
	struct block *superblock;
	unsigned char gcc_compile_flag;
	int nsyms;
	struct symbol syms[1];
};

struct blockvector {
	int nblocks;
	struct block *block[2];
};

struct blockvector *blockvector_for_pc_sect(register CORE_ADDR pc,
					    struct symtab *symtab)
{
  register struct block *b;
  register int bot, top, half;
  struct blockvector *bl;

  bl = symtab->blockvector;
  b = bl->block[0];

  bot = 0;
  top = bl->nblocks;

  while (top - bot > 1)
    {
      half = (top - bot + 1) >> 1;
      b = bl->block[bot + half];
      if (b->startaddr <= pc)
	bot += half;
      else
	top = bot + half;
    }

  while (bot >= 0)
    {
      b = bl->block[bot];
      if (b->endaddr > pc)
	{
	  return bl;
	}
      bot--;
    }
  return 0;
}

int main(void)
{
  struct block a = { 0, 0x10000, 0, 0, 1, 20 };
  struct block b = { 0x10000, 0x20000, 0, 0, 1, 20 };
  struct blockvector bv = { 2, { &a, &b } };
  struct symtab s = { &bv };

  struct blockvector *ret;

  ret = blockvector_for_pc_sect(0x500, &s);

  return 0;
}
