// { dg-do run }
// { dg-options "-O2" }

struct BOOL {
    int nVal:1, bSet:1;
    BOOL (int i) : nVal(i!=0), bSet(1) {}
};
struct Fill {
    void *d;
    Fill() : d(0) {}
    Fill( const Fill& ) {}
};
struct SvMetaSlot {
    Fill aGroupId;
    BOOL a8;
    SvMetaSlot() :
      a8(1) {}
    SvMetaSlot* MakeClone() const;
};

SvMetaSlot* SvMetaSlot::MakeClone() const { return new SvMetaSlot( *this ); }

extern "C" void abort(void);
int main()
{
  SvMetaSlot s; SvMetaSlot s2(s);
  if (s.a8.bSet != s2.a8.bSet)
    abort ();
  return 0;
}
