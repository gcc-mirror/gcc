// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>
// Origin: Bill Somerville <bill@classdesign.com>
// { dg-do compile }

int main()
{
  ( int() > int() );            // { dg-error "parse" "" { xfail *-*-* } }
  return 0;
}
