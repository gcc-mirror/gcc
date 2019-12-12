// PR c++/92201

int
foo (void (*p) ())
{
  return (*reinterpret_cast<int (*)()> (p)) ();
}
