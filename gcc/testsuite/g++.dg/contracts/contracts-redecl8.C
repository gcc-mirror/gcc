// test that contracts are matched on friend decls when the type is complete
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

struct T;

int both(int x, T *t) [[ pre: x > 0 ]] { return 0; }
int both2(int x, T *t) [[ pre: x > 0 ]];

template<typename Z>
int fn(int x, Z *z) [[ pre: x > 0 ]];

template<typename Z>
int fn2(int x, Z *z);

template<typename Z>
int fn3(int x, Z *z) [[ pre: x > 0 ]];

template<>
int fn3<T>(int x, T *z) [[ pre: x > 1 ]];

struct T
{
  friend int both2(int x, T *t) [[ pre: x > 1 ]] // { dg-error "mismatched" }
  {
    return 0;
  }

  friend int hidden(int x, T *t)
    [[ pre: x > 1 ]] [[ pre: t->pri > 0 ]]
  {
    return x;
  }

  /* cannot define friend spec, so we never get to matching contracts
  friend int fn<T>(int x, T *t)
    [[ pre: t->pri > 0 ]] { return 0; } // error defining explicit spec friend
    */

  // bad, general contracts must match general
  template<typename Z>
  friend int fn(int x, Z *z)
    [[ pre: z->pri > 1 ]] { return 0; } // { dg-error "mismatched" }

  // fine, can add contracts
  template<typename Z>
  friend int fn2(int x, Z *z)
    [[ pre: z->pri > 1 ]] { return 0; } // { dg-bogus "mismatched" }

  /* cannot declare without definition, so dup friend can't occur:
  friend int dup(int x, T *t)
    [[ pre: t->pri > 0 ]]; // error non-defining friend with contracts
  friend int dup(int x, T *t)
    [[ pre: t->pri > 1 ]]; // error non-defining friend with contracts
    */

  int x{1};
  private:
    int pri{-10};
};

int hidden(int x, T *t)
  [[ pre: x > 0 ]] [[ pre: t->pri > 1 ]]; // { dg-error "mismatched" }

