// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -Wno-pedantic} }
module;
# 5 __FILE__ 1
extern int counter;
# 7 "" 2
export module One;

export class Dyn
{
  int i;
public:
  Dyn () : i (++counter) {}
  operator int () const { return i; }
};

export Dyn one;
