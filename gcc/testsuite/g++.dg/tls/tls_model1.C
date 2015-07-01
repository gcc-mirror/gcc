// { dg-require-effective-target tls }
// { dg-options "-g" }

template <class T>
void f()
{
  static __thread int i __attribute ((tls_model ("local-exec")));
}
