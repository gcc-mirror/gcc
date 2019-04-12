// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module enUm;
// { dg-module-bmi "enUm" }

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

// { dg-final { scan-lang-dump {Written enum value '::Ben@1\(enUm\)'\[2\]} module } }
