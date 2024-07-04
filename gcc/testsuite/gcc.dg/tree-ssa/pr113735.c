// { dg-do compile { target bitint } }
// { dg-options "-O1" }

char b;
void bar (void);

#if __BITINT_MAXWIDTH__ >= 6110
void
foo (_BitInt(6110) j)
{
  for (;;)
    {
      _BitInt(10) k = b % j;
      for (j = 6; j; --j)
        if (k)
          bar ();
    }
}
#endif
