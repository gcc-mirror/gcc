/* { dg-do compile { target llp64 } } */
/* { dg-options "" } */

void foo (int i)
{
  void nested (void)
  {
    char arr[(1U << 31) + 4U];
    arr[i] = 0;
  }

  nested ();
}

