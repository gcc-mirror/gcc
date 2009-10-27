// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41020
// { dg-do compile }


class frok
{
  int this_errno;
  friend int fork (void); // { dg-error "previous declaration .*?C\\+\\+. linkage" }
};

extern "C" int
fork (void) // { dg-error "conflicts with new declaration .*?C. linkage" }}
{
  frok grouped;
  return grouped.this_errno;
}

