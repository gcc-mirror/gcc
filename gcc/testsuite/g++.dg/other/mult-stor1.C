// PR c++/20646
// Origin: Dan Rosen <dan.rosen@gmail.com>
// { dg-do compile }

struct A
{
  extern static int i;  // { dg-error "'static' specifier conflicts with 'extern'" }
};
