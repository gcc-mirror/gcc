// { dg-additional-options -fmodules-ts }

module foo;

void m ()
{
  foo ('a');
  foo (0);
}
