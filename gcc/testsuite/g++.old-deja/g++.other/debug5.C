// { dg-do assemble  }
// { dg-options "-g" }
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

