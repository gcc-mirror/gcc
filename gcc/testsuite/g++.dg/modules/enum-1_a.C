// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
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
