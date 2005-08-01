__inline void libc_name_p (void)
{
  enum { A = 1 };
}
void nothrow_libfn_p (void)
{
  libc_name_p ();
}
