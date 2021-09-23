// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
import  "pr99283-2_a.H";

inline namespace __cxx11 {
template<typename _CharT>
class collate
{
};
}

template<typename _CharT>
void Check ()
{
  typedef collate<char> __collate_type;
}






