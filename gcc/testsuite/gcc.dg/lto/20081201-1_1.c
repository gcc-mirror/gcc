extern int f (void);

extern inline int __attribute__ ((gnu_inline))
e_inline_baz (void)
{
 return 2 + f ();
}

int
foo (void)
{
 return e_inline_baz ();
}
