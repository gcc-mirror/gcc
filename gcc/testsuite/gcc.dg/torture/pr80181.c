/* { dg-do compile } */

int
nr (void)
{
}

void
it (int dl)
{
  int vp = 0;

  for (;;)
    {
      dl = vp ^ nr ();
      dl ^= vp;
      vp = 1;
    }
}
