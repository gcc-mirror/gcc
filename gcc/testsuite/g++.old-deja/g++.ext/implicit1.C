// test for implicit declaration
// Special g++ Options: -w -fpermissive

int
main ()
{
  return blarg ();
}

extern "C" int
blarg (...)
{
  return 0;
}
