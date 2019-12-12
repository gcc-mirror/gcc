// { dg-do compile { target c++11 } }

template<typename SX, typename ...XE> void
fk (XE..., int);

void
w9 (void)
{
  fk<int> (0);
}
