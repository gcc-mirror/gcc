// PR c++/45401
// { dg-options -std=c++11 }

typedef int &__restrict restrictLvref;
typedef restrictLvref &&rvrefToRestrictLvref;
typedef restrictLvref rvrefToRestrictLvref;
