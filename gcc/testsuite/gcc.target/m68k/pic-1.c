/* { dg-do compile { target m68k-*-* fido-*-* } }  */
/* { dg-options "-O2 -fpic" }  */

extern void Foo (void *);

char *ary[] = {"a", "b", "c", "d", "e"};

void Bar (void)
{
  int cnt = 0;

  for (cnt = 0; cnt < 4;  ++cnt)
    {
      char *ptr = ary[cnt];

      Foo (&ptr);
    }
}
