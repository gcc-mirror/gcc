static void __attribute__((noinline))
bar (void)
{
}

void
foo (void)
{
  bar ();
}
