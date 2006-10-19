extern void abort(void);
typedef long GLint;
void aglChoosePixelFormat (const GLint *);

void
find (const int *alistp)
{
  const int *blist;
  int list[32];
  if (alistp)
    blist = alistp;
  else
    {
      list[3] = 42;
      blist = list;
    }
  aglChoosePixelFormat ((GLint *) blist);
}

void
aglChoosePixelFormat (const GLint * a)
{
  int *b = (int *) a;
  if (b[3] != 42)
    abort ();
}

int
main (void)
{
  find (0);
  return 0;
}
