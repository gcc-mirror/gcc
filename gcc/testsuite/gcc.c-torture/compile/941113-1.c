typedef void foo (void);

f (x)
{
  if (x)
    {
      const foo* v;
      (*v)();
    }
  else
    g (0);
}
