#include <stdio.h>

struct S0 {
  virtual void is_kind_of_S1 () const { fprintf (stderr, "%p\n", this); }
  virtual void dummy () { }
};

struct S1 : virtual public S0 {
  virtual void is_kind_of_S1 () const { fprintf (stderr, "%p\n", this); }
  virtual void dummy () { }
};

struct S2 : virtual public S0 {
  virtual void dummy () { }
};

struct S3 : public S2, public S1 {
  virtual void dummy () { }
};

static struct S0 *var = new S3 ();

int main () {
  void **vp = (void **) var;

  printf ("%d\n", sizeof (S3));
  printf ("%p\n", vp[0]);
  printf ("%p\n", vp[1]);

  // vp[1] is the vtable we care about.
  void **vp2 = (void**) vp[1];
  printf ("%p\n", vp2[0]);
  printf ("%p\n", vp2[1]);
  printf ("%p\n", vp2[2]);
  printf ("%p\n", vp2[3]);
  
  var->is_kind_of_S1();
  return 0;
}
