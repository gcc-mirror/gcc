// { dg-additional-options {-fmodule-header -fno-module-lazy} }
// { dg-module-cmi {} }
import  "pr99239_a.H";
       
inline void
  _M_remove_reference() throw()
{
  try
    { }
  catch(...)
    { }
}
