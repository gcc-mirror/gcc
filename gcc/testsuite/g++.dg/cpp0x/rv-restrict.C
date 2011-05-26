// PR c++/45401
// { dg-options -std=c++0x }

typedef int &__restrict restrictLvref;
typedef restrictLvref &&rvrefToRestrictLvref;
typedef restrictLvref rvrefToRestrictLvref;
