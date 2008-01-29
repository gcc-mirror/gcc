// PR c++/35007

struct AffEntry
{
  union {
    char base[256];
  } conds;
};

struct PfxEntry
: public AffEntry
{
  PfxEntry()
  {
    sizeof(conds.base[0]);
  }
};
