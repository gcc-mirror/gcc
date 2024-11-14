
void longj (void *buf)
{
  __builtin_longjmp (buf, 1);
}

void foo (void);
void bar (void);

void setj (void *buf)
{
  if (__builtin_setjmp (buf))
    foo ();
  else
    bar ();
}
