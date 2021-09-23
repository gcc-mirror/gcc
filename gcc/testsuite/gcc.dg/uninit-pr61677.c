/* PR tree-optimization/61677 - False positive with -Wmaybe-uninitialized
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void *xmalloc (void);

struct menu { struct menu *parent; };

struct jump_key { int offset; };

void f (struct menu *menu)
{
  int i;
  struct menu *submenu[8], *location;
  struct jump_key *jump;
  location = menu;
  for (i = 0; menu && i < 8; menu = menu->parent)
    submenu[i++] = menu;
  if (location)
    jump = xmalloc ();
  while (--i >= 0) {
    menu = submenu[i];
    if (location)
      jump->offset = 42;
  }
}
