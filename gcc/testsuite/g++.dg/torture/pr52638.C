// { dg-do compile }
// { dg-options "-ftree-vectorize" }

void
bar (bool * b, bool * e, bool t)
{
  while (b < e)
    *b++ = t;
}

void
foo (bool * b, bool * e, bool t)
{
  bar (b, e, true);
}
