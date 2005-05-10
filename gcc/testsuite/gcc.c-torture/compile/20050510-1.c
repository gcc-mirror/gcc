void bar (int k)
{
  void *label = (k) ? &&x : &&y;
  if (k)
    goto *label;

x:
  if (k)
    dont_remove ();
y:
  return;
}
