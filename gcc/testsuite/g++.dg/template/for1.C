// PR c++/47388
// { dg-do compile }
// { dg-options "-fno-for-scope" }

template <int>
void
foo ()
{
  int i;
  for (i = 0; i < 16; i++)
    ;
  for (int j = 0; j < 16; j++)
    ;
  if (j != 16)
    for (;;)
      ;
}

void
bar ()
{
  foo <0> ();
}
