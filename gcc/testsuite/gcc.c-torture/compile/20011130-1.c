struct S { int i; };
extern struct S x[];
char *bar (const struct S *);
void foo (void)
{
  bar (x);
}
