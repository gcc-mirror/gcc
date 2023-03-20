// PR c++/109164
// { dg-do run { target c++11 } }
// { dg-options "-O2" }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }
// { dg-additional-sources "thread_local14-aux.cc" }

extern thread_local const int t;
bool bar (int);

bool
baz ()
{
  while (1)
    {
      if (!bar (t))
        return false;
    }
}
