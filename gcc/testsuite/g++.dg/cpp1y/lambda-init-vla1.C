// PR c++/88394
// { dg-do compile { target c++14 } }
// { dg-options "" }

void crash_me(unsigned short sz)
{
  if (sz == 0) return;

  short iov[sz];
  auto fce = [&iv = iov](short value) { iv[0] = 0; };
  fce(1);
}
