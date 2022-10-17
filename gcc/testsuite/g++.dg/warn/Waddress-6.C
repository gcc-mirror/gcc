/* PR c/102103 - missing warning comparing array address to null
   { dg-do compile }
   Verify -Waddress for member arrays of structs and notes.
   { dg-options "-Wall" } */

#if __cplusplus < 201103L
# define nullptr __null
#endif

void T (bool);

struct A
{
  int n;
  int ia[];                   // { dg-message "'A::ia' declared here" }
};

struct B
{
  A a[3];                     // { dg-message "'B::a' declared here" }
};

struct C
{
  B b[3];                     // { dg-message "'C::b' declared here" }
};

struct D
{
  C c[3];                     // { dg-message "'D::c' declared here" }
};


void test_waddress_1d ()
{
  D d[2];                     // { dg-message "'d' declared here" }

  T (d);                      // { dg-warning "address of 'd'" }
  T (d == nullptr);           // { dg-warning "address of 'd'" }
  T (&d);                     // { dg-warning "address of 'd'" }
  T (d->c);                   // { dg-warning "address of 'D::c'" }
  T (d->c != nullptr);        // { dg-warning "address of 'D::c'" }
  T (d->c->b);                // { dg-warning "address of 'C::b'" }
  T (d->c[1].b->a);           // { dg-warning "address of 'B::a'" }
  T (d->c->b[2].a->ia);       // { dg-warning "address of 'A::ia'" }

  if (d->c->b[2].a[1].ia)     // { dg-warning "address of 'A::ia'" }
    T (0);

  if (bool b = d->c->b[1].a)  // { dg-warning "address of 'B::a'" }
    T (b);

  /* The following is represented as a declaration of P followed
     by an if statement and so it isn't diagnosed.  It's not clear
     that it should be since the pointer is then used.
       void *p = d->c->b[2].a;
       if (p) ...
  */
  if (void *p = d->c->b[2].a) // { dg-warning "address of 'A::ia'" "" { xfail *-*-* } }
    T (p);
}


void test_waddress_2d (int i)
{
  D d[2][3];                  // { dg-message "'d' declared here" }

  T (d);                      // { dg-warning "address of 'd'" }
  T (d == nullptr);           // { dg-warning "address of 'd'" }
  T (&d);                     // { dg-warning "address of 'd'" }
  T (*d);                     // { dg-warning "address of 'd'" }
  T (d[1] != nullptr);        // { dg-warning "address of 'd'" }
  T (&d[1]->c);               // { dg-warning "address of 'D::c'" }
  T (d[1]->c);                // { dg-warning "address of 'D::c'" }
  T (d[1]->c == nullptr);     // { dg-warning "address of 'D::c'" }
  T (d[i]->c[1].b);           // { dg-warning "address of 'C::b'" }
  T ((*(d + i))->c->b->a);    // { dg-warning "address of 'B::a'" }
  T (d[1][2].c->b->a->ia);    // { dg-warning "address of 'A::ia'" }
}
