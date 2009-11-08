// { dg-do compile }
// { dg-options "-fshow-column" }

ACE_Process_Descriptor::ACE_Process_Descriptor () :
  // { dg-error "does not name a type" "no type" { target *-*-* } 4 }
  process_ (0)
{
}
