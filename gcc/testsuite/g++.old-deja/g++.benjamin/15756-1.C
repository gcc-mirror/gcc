// { dg-do assemble  }
// { dg-options "-Wsign-promo" }
// 981203 bkoz
// g++/15756  test1

enum e_value { first = 0, next = 30 };

struct sanjuan {
  sanjuan(int value);
  sanjuan(unsigned value);
  friend sanjuan operator&(const sanjuan& x, const sanjuan& y);
  friend int operator!=(const sanjuan& x, const sanjuan& y);
};

extern void mod_enum(e_value*);
extern int a;

void foo(void) {
  e_value mod = first;
  mod_enum(&mod);
  if (mod != next)
    ++a;
}













