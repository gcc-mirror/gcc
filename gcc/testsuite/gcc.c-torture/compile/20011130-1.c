extern struct S x[];
struct S { int i; };
char *bar (const struct S *);
void foo (void)
{
  bar (x);
}
