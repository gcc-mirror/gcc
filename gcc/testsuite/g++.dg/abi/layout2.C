// Red Hat bugzilla 65210
// { dg-do run }

struct A {
    int a;
};

struct B : public virtual A {};

struct C {
  long double c;
};

struct D : public virtual C {
    int d;
};

struct E : public B, public D {
    int e;
};

E e;

/* The layout of E should begin with the B-in-E vtable pointer, followed by
   the D-in-E vtable pointer.  The bug was that we used to pad out the D
   fields for long double alignment.  */

int main ()
{
  D* dp = &e;
  unsigned long d_offset = ((char*)dp) - ((char*) &e);
  return (d_offset != sizeof(void *));
}
