// PR debug/53860
// { dg-do compile }
// { dg-options "-fkeep-inline-functions -fdebug-types-section" }

void
foo ()
{
  struct S
  {
    S ()
    {
    }
  };
}
