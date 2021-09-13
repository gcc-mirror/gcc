/* PR c++/98646 - spurious -Wnonnull calling a member on the result
   of static_cast
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct A { virtual ~A (); };
struct B
{
  virtual ~B ();
  B* bptr ();
  B& bref ();
};

struct C: A, B { virtual ~C (); void g () const; };


void c_cast_C_ptr (B *p)
{
  ((C*)p->bptr ())->g ();
}

void c_cast_const_C_ptr (B *p)
{
  ((const C*)p->bptr ())->g ();
}

void static_cast_C_ptr (B *p)
{
  static_cast<C*>(p->bptr ())->g ();
}

void static_cast_const_C_ptr (B *p)
{
  /* The static_cast can't fail so verify that no warning is issued
     here, even though GCC emits a null check for its argument.  */
  static_cast<const C*>(p->bptr ())->g ();    // { dg-bogus "\\\[-Wnonnull" }
}

void dynamic_cast_C_ptr (B *p)
{
  /* Unlike static_cast, dynamic cast may return null even for a nonnull
     operand but detecting assumptions to the contrary isn't -Wnonnull's
     purpose.  Verify -Wnonnull isn't issued, either for the implicitly
     emitted null check or for other reasons (the latter may be worth
     warning for by some other warning).  See also pr99251.  */
  dynamic_cast<C*>(p->bptr ())->g ();         // { dg-bogus "\\\[-Wnonnull" }
}

void dynamic_cast_const_C_ptr (B *p)
{
  dynamic_cast<const C*>(p->bptr ())->g ();   // { dg-bogus "\\\[-Wnonnull" }
}


void c_cast_C_ref (B *p)
{
  ((C&)p->bref ()).g ();
}

void c_cast_const_C_ref (B *p)
{
  ((const C&)p->bref ()).g ();
}

void static_cast_C_ref (B *p)
{
  static_cast<C&>(p->bref ()).g ();
}

void static_cast_const_C_ref (B *p)
{
  static_cast<const C&>(p->bref ()).g ();
}

void dynamic_cast_C_ref (B *p)
{
  /* The dynamic_cast fails by throwing an exception so verify that
     no warning is issued.  */
  dynamic_cast<C&>(p->bref ()).g ();
}

void dynamic_cast_const_C_ref (B *p)
{
  dynamic_cast<const C&>(p->bref ()).g ();
}


struct D: B, A { virtual ~D (); void g () const; };

void c_cast_D_ptr (B *p)
{
  ((D*)p->bptr ())->g ();
}

void c_cast_const_D_ptr (B *p)
{
  ((const D*)p->bptr ())->g ();
}

void static_cast_D_ptr (B *p)
{
  static_cast<D*>(p->bptr ())->g ();
}

void static_cast_const_D_ptr (B *p)
{
  /* The static_cast can't fail so verify that no warning is issued
     here, even though GCC emits a null check for its argument.  */
  static_cast<const D*>(p->bptr ())->g ();    // { dg-bogus "\\\[-Wnonnull" }
}

void dynamic_cast_D_ptr (B *p)
{
  /* Unlike static_cast, dynamic cast may return null even for a nonnull
     operand but detecting assumptions to the contrary isn't -Wnonnull's
     purpose.  Verify -Wnonnull isn't issued, either for the implicitly
     emitted null check or for other reasons (the latter may be worth
     warning for by some other warning).  See also pr99251.  */
  dynamic_cast<D*>(p->bptr ())->g ();         // { dg-bogus "\\\[-Wnonnull" }
}

void dynamic_cast_const_D_ptr (B *p)
{
  dynamic_cast<const D*>(p->bptr ())->g ();   // { dg-bogus "\\\[-Wnonnull" }
}
