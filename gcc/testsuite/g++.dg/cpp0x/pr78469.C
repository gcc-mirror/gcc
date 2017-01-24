// { dg-do compile { target c++11 } }
// PR78469, bogus error about inaccessible dtor.

struct no_destr {
  no_destr() = default;

protected:
  ~no_destr() = default;
};

void *Foo ()
{
  return new no_destr ();
}
