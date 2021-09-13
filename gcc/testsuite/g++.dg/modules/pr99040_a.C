// PR c++/99040
// { dg-additional-options -fmodules-ts }
export  module  format;
// { dg-module-cmi format }

export namespace NS
{
void Format ();
}
