/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O3 -mx32 -maddress-mode=long -fpic" } */
/* { dg-final { scan-assembler-not "movl\[\\t \]*%.*,\[\\t \]*-1073742592\\(%r(.x|.i|.p|\[1-9\]*)\\)" } } */

typedef int int32_t;
typedef unsigned int uint32_t;
typedef uint32_t Elf32_Word;
typedef int32_t Elf32_Sword;
typedef uint32_t Elf32_Addr;
typedef struct {
  Elf32_Sword d_tag;
  union {
    Elf32_Word d_val;
    Elf32_Addr d_ptr;
  } d_un;
} Elf32_Dyn;
struct link_map {
  Elf32_Dyn *l_ld;
  Elf32_Dyn *l_info[34 + 16 + 3 + 12 + 11];
};
void
elf_get_dynamic_info (struct link_map *l)
{
  Elf32_Dyn *dyn = l->l_ld;
  Elf32_Dyn **info = l->l_info;
  typedef Elf32_Word d_tag_utype;
  while (dyn->d_tag != 0) {
    if ((d_tag_utype) (0x6ffffeff - dyn->d_tag) < 11)
      info[(0x6ffffeff - dyn->d_tag) + 34 + 16 + 3 + 12] = dyn;
    ++dyn;
  }
}
