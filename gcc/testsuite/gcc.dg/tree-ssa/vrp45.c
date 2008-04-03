/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);

void foo (int i)
{
  if (i > -128 && i < 127)
    {
      unsigned char k = i;
      if (k == 0x80)
        link_error ();
      if (k == 0x7f)
        link_error ();
    }
}

int main() { return 0; }

