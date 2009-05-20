/* { dg-do run } */
/* { dg-options "-O1 " } */

extern void abort (void);

static void __attribute__((always_inline))
reverse(int *first, int *last)
{
  if (first == last--) 
    return;
  while (first != last)
    {
      int t = *first;
      *first = *last;
      *last = t;
      if (++first == last--)
        break;
    }
}

int main()
{
  int seq[] = { 1, 2, 3, 4, 5, 6, 7, 8 };

  reverse(seq, seq + 8);
  if (seq[3] != 5 || seq[4] != 4)
    abort ();

  return 0;
}
