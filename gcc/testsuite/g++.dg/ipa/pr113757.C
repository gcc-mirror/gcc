// { dg-do compile }
// { dg-options "-O2 -fPIC" }
// { dg-require-effective-target fpic }

long size();
struct ll {  virtual int hh();  };
ll  *slice_owner;
int ll::hh() { __builtin_exit(0); }
int nn() {
  if (size())
    return 0;
  return slice_owner->hh();
}
int (*a)() = nn;
