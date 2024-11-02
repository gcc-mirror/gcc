/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpic" } */

struct link_map
{
  struct link_map *l_next;
};
struct rtld_global
{
  struct link_map *_ns_loaded;
  char buf[4096];
  struct link_map _dl_rtld_map;
};
extern struct rtld_global _rtld_global;
static int _dlfo_main __attribute__ ((section (".data.rel.ro"), used));
void
_dlfo_process_initial (int ns)
{
  for (struct link_map *l = _rtld_global._ns_loaded; l != ((void *)0);
       l = l->l_next)
    if (l == &_rtld_global._dl_rtld_map)
      asm ("");
}
