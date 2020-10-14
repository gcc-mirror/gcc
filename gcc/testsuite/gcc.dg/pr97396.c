// { dg-do compile }
// { dg-options "-O1 -ftree-vrp" }
// { dg-additional-options "-m32" { target { i?86-*-* x86_64-*-* } } }

unsigned int
po (char *os, unsigned int al)
{
  for (;;)
    {
      int qx = 0;

      while (al < 1)
        {
          char *cw;

          cw = os + qx;
          if (cw)
            return al + qx;

          qx += sizeof *cw;
        }
    }
}
