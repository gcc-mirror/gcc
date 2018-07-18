// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41020

// { dg-options "" }
// { dg-do compile }

class frok
{
  int this_errno;
  friend int fork (void);
};

void
foo ()
{
  fork (); // { dg-error "was not declared in this scope" }
}
