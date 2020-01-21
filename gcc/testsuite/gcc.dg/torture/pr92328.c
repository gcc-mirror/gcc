/* { dg-do compile } */
/* { dg-additional-options "-ftree-pre -Wno-div-by-zero" } */

int nt;

void
ja (int os)
{
  int *ku = &os, *id = &os;
  unsigned int qr = 0;

  for (;;)
    {
      if (os == *ku)
        {
          *id = 0;
          qr += os != *ku;
          id = &qr;
        }

      *id &= qr;

      if (os != 0)
        {
          nt /= 0;
          ku = &qr;
        }
    }
}
