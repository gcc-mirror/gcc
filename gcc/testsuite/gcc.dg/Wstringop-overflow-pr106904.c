/* { dg-do compile } */
/* { dg-options "-O2 -Wstringop-overflow -fno-vect-cost-model" } */

struct windowpos
{
  int hwnd;
  int hwnd2;
};

struct packed_windowpos
{
  int hwnd;
  int pad1;
  int hwnd2;
  int pad2;
};

struct packed_structs
{
  struct packed_windowpos wp;
};

void func(struct packed_structs *ps)
{
  struct windowpos wp;

  wp.hwnd = ps->wp.hwnd;
  wp.hwnd2 = ps->wp.hwnd2;
  __builtin_memcpy(&ps->wp, &wp, sizeof(wp)); /* { dg-bogus "into a region" } */
}
