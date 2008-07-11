// { dg-do compile }
// { dg-options "-fshow-column" }

ACE_Process_Descriptor::ACE_Process_Descriptor () : // { dg-error "" }
  process_ (0) // { dg-error "3: error: only constructors take base initializers" }
{
}
