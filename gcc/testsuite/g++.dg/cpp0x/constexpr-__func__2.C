// PR c++/70353
// { dg-do compile { target c++11 } }

constexpr const char* ce ()
{
  return __func__;
}

const char *c = ce();

#define SA(X) static_assert((X),#X)
SA(ce()[0] == 'c');
