// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

template<info val>
void
f ()
{
  [:val:];	  // { dg-error "use of local variable with automatic storage from containing function" }
  [:val:] = {};	  // { dg-error "use of local variable with automatic storage from containing function" }
  /* This is ill-formed because:
     - the splice-expression designates b,
     - the splice-expression names b ([basic.def.odr]/5),
     - the splice-expression odr-uses b ([basic.def.odr]/5),
     - however, the local entity b is not odr-usable from the scope inhabited
       by the splice-expression ([basic.def.odr]/10.2).  Since the local
       entity b is odr-used within a scope where b is not odr-usable,
       the program is ill-formed (also [basic.def.odr]/10).  */
  [:val:] z;  // { dg-error "use of local variable with automatic storage from containing function|expected" }
  float a = [:val:]; // { dg-error "use of local variable with automatic storage from containing function" }
}

void
g ()
{
  float b = 2.5f;
  f<^^b>();
}
