// { dg-additional-options "-fmodules-ts" }
module One;

pad::~pad ()
{
}

int base::getter () const
{
  return b;
}
