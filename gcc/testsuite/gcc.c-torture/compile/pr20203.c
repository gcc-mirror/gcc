void *memset (void *, int, unsigned long);

typedef struct bfd_section
{
  unsigned long size;
  unsigned char *contents;
} asection;

int
_bfd_mips_elf_finish_dynamic_sections (asection *s)
{
  long long dummy_offset;
  dummy_offset = s->size - 16;
  memset (s->contents + dummy_offset, 0, 16);
  return 1;
}
