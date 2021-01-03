// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename T>
inline void Foo ()
{
  {
    void Frob ();
  }
  {
    void Frob ();
  }
  {
    void Frob ();
  }
}
