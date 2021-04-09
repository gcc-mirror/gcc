// { dg-module-do run }
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

enum { _S_chunk_size = 7 };

template<typename T> T getter ()
{
  return _S_chunk_size;
}
