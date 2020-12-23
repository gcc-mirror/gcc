// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename T>
inline void Foo ()
{
  {
    extern int EXTERN_;
    EXTERN_++;
  }
  {
    extern int EXTERN_;
    EXTERN_++;
  }
  {
    extern int EXTERN_;
    EXTERN_++;
  }
}
