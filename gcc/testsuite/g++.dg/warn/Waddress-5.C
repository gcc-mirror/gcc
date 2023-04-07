/* PR c/102103 - missing warning comparing array address to null
   { dg-do compile }
   { dg-options "-Wall" } */

#if __cplusplus < 201103L
# define nullptr __null
#endif

struct A
{
  void f ();
  virtual void vf ();
  virtual void pvf () = 0;

  static void sf ();

  int *p;
  int a[2];
};

void T (bool);

void warn_memptr_if ()
{
  // Exercise warnings for addresses of nonstatic member functions.
  // On targets with TARGET_PTRMEMFUNC_VBIT_LOCATION ==
  // ptrmemfunc_vbit_in_delta, cp_build_binary_op recurses to compare
  // the pfn from the ptrmemfunc with null, so we get two warnings.
  // This matches both.  ??? Should we disable one of them?
  if (&A::f == 0)         // { dg-warning "A::f" }
    T (0);

  if (&A::vf)             // { dg-warning "-Waddress" }
    T (0);

  if (&A::pvf != 0)       // { dg-warning "-Waddress" }
    T (0);

  // Exercise warnings for addresses of static member functions.
  if (&A::sf == 0)        // { dg-warning "-Waddress" }
    T (0);

  if (&A::sf)             // { dg-warning "-Waddress" }
    T (0);

  // Exercise warnings for addresses of nonstatic data members.
  if (&A::p == 0)         // { dg-warning "the address '&A::p'" }
    T (0);

  if (&A::a == nullptr)   // { dg-warning "-Waddress" }
    T (0);
}

void warn_memptr_bool ()
{
  // Exercise warnings for addresses of nonstatic member functions.
  T (&A::f == 0);         // { dg-warning "-Waddress" }
  T (&A::vf);             // { dg-warning "-Waddress" }
  T (&A::pvf != 0);       // { dg-warning "-Waddress" }

  // Exercise warnings for addresses of static member functions.
  T (&A::sf == 0);        // { dg-warning "-Waddress" }
  T (&A::sf);             // { dg-warning "-Waddress" }

  // Exercise warnings for addresses of nonstatic data members.
  T (&A::p == 0);         // { dg-warning "-Waddress" }
  T (&A::a == nullptr);   // { dg-warning "-Waddress" }
}


/* Verify that no warnings are issued for a dependent expression in
   a template.  */

template <int>
struct B
{
  // This is why.
  struct F { void* operator& () const { return 0; } } f;
};

template <class Type, int N>
void nowarn_dependent (Type targ)
{
  T (&Type::x == 0);
  T (&targ == 0);

  Type tarr[1];
  T (&tarr[0] == nullptr);

  T (&B<N>::f == 0);

  /* Like in the case above, the address-of operator could be a member
     of B<N>::vf that returns zero.  */
  T (&B<N>::vf);
  T (&B<N>::pvf != 0);
  T (&B<N>::p == 0);
  T (&B<N>::a == 0);
}


/* Verify that in an uninstantiated template warnings are not issued
   for dependent expressions but are issued otherwise.  */

template <class Type>
void warn_non_dependent (Type targ, Type *tptr, int i)
{
  /* The address of a pointer to a dependent type cannot be null but
     the warning doesn't have a chance to see it.  */
  T (&tptr == 0);       // { dg-warning "-Waddress" "pr102378" { xfail *-*-* } }
  T (&i == 0);          // { dg-warning "-Waddress" }

  int iarr[1];
  T (&iarr == 0);       // { dg-warning "-Waddress" }
  T (&*iarr != 0);      // { dg-warning "-Waddress" "pr102378" { xfail *-*-* } }
  T (&iarr[0] == 0);    // { dg-warning "-Waddress" }

  Type tarr[1];
  T (&tarr == nullptr);   // { dg-warning "-Waddress" "pr102378" { xfail *-*-* } }
}
