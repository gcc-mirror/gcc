// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
import  "pr99283-2_a.H";

template<typename _CharT>
class __cxx11::collate
{
};

template<typename _CharT>
void Check ()
{
  typedef collate<char> __collate_type;
}






