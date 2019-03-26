// { dg-do compile { target { c++14 && comdat_group } } }
// { dg-additional-options -fno-inline }

inline auto lamby () 
{
  return [] {};
}

void direct ()
{
  lamby ()();
}

void indirect ()
{
  void (*invoke) () = lamby ();

  invoke ();
}

// The call operator and the static invoker should be comdat, but not
// the same group.  (that would be a compiler incompatibility)

// { dg-final { scan-assembler ".section\[\t ]*.text._ZZ5lambyvENKUlvE_clEv,\[^\n\r]*,_ZZ5lambyvENKUlvE_clEv,comdat" { target { { ! *-*-solaris2.* } || { gas } } } } }
// { dg-final { scan-assembler ".section\[\t ]*.text._ZZ5lambyvENUlvE_4_FUNEv,\[^\n\r]*,_ZZ5lambyvENUlvE_4_FUNEv,comdat" { target { { ! *-*-solaris2.* } || { gas } } } } }
// { dg-final { scan-assembler ".group\[\t \]*_ZZ5lambyvENKUlvE_clEv,\[^\n\r\]*,#comdat" { target { *-*-solaris2.* && { ! gas } } } } }
// { dg-final { scan-assembler ".group\[\t \]*_ZZ5lambyvENUlvE_4_FUNEv,\[^\n\r\]*,#comdat" { target { *-*-solaris2.* && { ! gas } } } } }
