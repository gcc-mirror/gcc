// { dg-do compile }
// { dg-options "-fshow-column" }

ACE_Process_Descriptor::ACE_Process_Descriptor () : // { dg-error "declared" "declared" }
  // { dg-error "no type" "no type" { target *-*-* } 4 }
  process_ (0) // { dg-error "3: only constructors take base initializers" }
{
}
