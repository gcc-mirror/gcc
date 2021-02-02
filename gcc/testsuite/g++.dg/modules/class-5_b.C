// { dg-additional-options "-fmodules-ts" }
module One;

int base::getter () const
{
  return b;
}
