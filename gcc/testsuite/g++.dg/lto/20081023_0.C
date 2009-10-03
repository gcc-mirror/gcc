// { dg-lto-do link }
extern inline void __attribute__ ((__always_inline__)) func (void)
{
}

void
f (void)
{
  func ();
}

main()
{
}
