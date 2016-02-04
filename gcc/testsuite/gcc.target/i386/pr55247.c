/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mno-sse -mno-mmx -mx32 -maddress-mode=long" } */

typedef unsigned int uint32_t;
typedef uint32_t Elf32_Word;
typedef uint32_t Elf32_Addr;
typedef struct {
  Elf32_Word st_name;
  Elf32_Addr st_value;
  Elf32_Word st_size;
  unsigned char st_other;
} Elf32_Sym;
typedef struct {
  Elf32_Word r_info;
}
Elf32_Rela;
typedef struct {
  union {
    Elf32_Addr d_ptr;
  }
  d_un;
} Elf32_Dyn;
struct link_map   {
  Elf32_Dyn *l_info[34];
};
extern void symbind32 (Elf32_Sym *);
void
_dl_profile_fixup (struct link_map *l, Elf32_Word reloc_arg)
{
  const Elf32_Sym *const symtab  = (const void *) l->l_info[6]->d_un.d_ptr;
  const Elf32_Rela *const reloc  = (const void *) (l->l_info[23]->d_un.d_ptr + reloc_arg * sizeof (Elf32_Rela));
  Elf32_Sym sym = symtab[(reloc->r_info) >> 8];
  symbind32 (&sym);
}
