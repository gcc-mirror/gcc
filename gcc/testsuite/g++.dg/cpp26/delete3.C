// C++26 P3144R2 - Deleting a Pointer to an Incomplete Type Should be Ill-formed
// { dg-do compile { target c++26 } }
// { dg-options "-fpermissive" }

struct S;	// { dg-message "forward declaration of 'struct S'" }
struct T;	// { dg-message "forward declaration of 'struct T'" }
struct U;	// { dg-message "forward declaration of 'struct U'" }

void
foo (S *p, T *q, U *r, S *s, T *t, U *u)
{
  delete p;	// { dg-warning "operator 'delete' used on incomplete type" }
  delete q;	// { dg-warning "operator 'delete' used on incomplete type" }
  delete r;	// { dg-warning "operator 'delete' used on incomplete type" }
  delete[] s;	// { dg-warning "operator 'delete \\\[\\\]' used on incomplete type" }
  delete[] t;	// { dg-warning "operator 'delete \\\[\\\]' used on incomplete type" }
  delete[] u;	// { dg-warning "operator 'delete \\\[\\\]' used on incomplete type" }
}

struct S
{
  int s;
};

struct T
{
  int t;
  ~T () {}
};

struct U
{
  int u;
  void operator delete (void *) noexcept;
  void operator delete[] (void *) noexcept;
};
