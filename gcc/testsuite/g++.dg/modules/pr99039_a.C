// PR c++/99039
// { dg-additional-options -fmodules-ts }
export  module  format;
// { dg-module-cmi format }

export namespace NS
{
void Format ();
}
