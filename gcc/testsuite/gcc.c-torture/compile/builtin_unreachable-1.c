void bar (const char *);
void foo (void)
{
  bar ("foo");
  __builtin_unreachable ();
}
