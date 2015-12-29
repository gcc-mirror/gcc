/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -mx32 -maddress-mode=long -fpic" } */

typedef int int32_t;
typedef unsigned int uint32_t;
typedef int32_t Elf32_Sword;
typedef struct
{
  Elf32_Sword d_tag;
} Elf32_Dyn;
struct link_map
{
  Elf32_Dyn *l_ld;
  Elf32_Dyn *l_info[34];
};
extern struct link_map _dl_rtld_map __attribute__ ((visibility ("hidden")));
static void elf_get_dynamic_info (struct link_map *l)
{
  Elf32_Dyn *dyn = l->l_ld;
  Elf32_Dyn **info;
  info = l->l_info;
  while (dyn->d_tag != 0)
    {
      if ((uint32_t) (0x6ffffeff - dyn->d_tag) < 11)
	info[0x6ffffeff - dyn->d_tag + 12] = dyn;
      ++dyn;
    }
}
void
foo (void)
{
  elf_get_dynamic_info (&_dl_rtld_map);
}
