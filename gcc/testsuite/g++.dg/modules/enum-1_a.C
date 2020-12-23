// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module enUm;
// { dg-module-cmi "enUm" }

export enum Bill
{
  Zero,
  One,
  Three = 3
};

export enum class Ben
{
  Zero,
  Two = 2,
  Three
};

export inline Ben func1 ()
{
  return Ben::Three;
}

export inline Ben func2 ()
{
  return Ben (4);
}

// { dg-final { scan-lang-dump-times {Written enum value '::Ben::Three'} 2 module } }
