/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);

void test1(int i)
{
  if (i >= -5 && i <= 8)
    {
      unsigned int j = i + 1;
      if (j == -5)
	link_error ();
      if (j == 10)
	link_error ();
    }
}

void test2(unsigned int i)
{
  if (i >= -5 || i <= 8)
    {
      int j = i;
      if (j == -6)
	link_error ();
      if (j == 9)
	link_error ();
    }
}

int main() { return 0; }
