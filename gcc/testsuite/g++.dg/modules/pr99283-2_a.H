// PR 99283 part 2 ICE on definition with qualified-name
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

inline namespace __cxx11 {

template<typename _CharT>
class collate;

}
       

