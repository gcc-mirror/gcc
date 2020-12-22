// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
export module tom.riddle;
// { dg-module-cmi tom.riddle }

export inline auto One (int a)
{
  return [=] (int b) { return a + b; };
}

// Look Ma! this isn't inline!
export auto Two (int a)
{
  return [=] (int b) { return a * b; };
}
