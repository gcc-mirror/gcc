/* PR middle-end/83185 */
/* { dg-do link } */
/* { dg-options "-O1" } */

struct f {
  void (*func)(void);
};

extern void link_error(void);
extern int printf(const char *f, ...);

static inline struct f *gimme_null(struct f *result)
{
  return 0;
}

int main(int argc, char **argv)
{
  struct f *x = gimme_null(&(struct f) { .func = link_error });
  printf("%p", x);
}
