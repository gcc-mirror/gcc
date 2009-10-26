// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/41020
// { dg-do compile }

extern "C"
{
  int fork (void);
}

class frok
{
  int this_errno;
  friend int fork (void);
};

extern "C" int
fork (void)
{
  frok grouped;
  return grouped.this_errno;
}
