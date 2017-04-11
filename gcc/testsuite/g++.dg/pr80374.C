// { dg-do compile }
// { dg-options "-O1 -std=c++11" }
void a (const char *, const char *, int, const char *)
  __attribute__ ((__noreturn__));
template <typename b, int>
void
c () try
  {
    throw;
  }
catch (b d)
  {
    if (d)
      a ("", "", 2, __PRETTY_FUNCTION__);
  }
void
foo ()
{
  using e = decltype (nullptr);
  c<volatile e, true> ();
}

