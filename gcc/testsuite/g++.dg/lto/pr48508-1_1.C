static void
bar (void)
{
  extern void foo (int);
  foo (0);
}
int main()
{
  bar ();
}
