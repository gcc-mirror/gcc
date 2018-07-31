// PR c++/86569
// { dg-do compile }
// { dg-options "-fcompare-debug=-Wnonnull-compare" }

bool b;

int
main ()
{
  return ((!b) != 0);
}
