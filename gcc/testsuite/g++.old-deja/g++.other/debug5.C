// Build don't link:
// Special g++ Options: -g
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  ~S ();
};

void f ()
{
 t:
  S s3;
}

