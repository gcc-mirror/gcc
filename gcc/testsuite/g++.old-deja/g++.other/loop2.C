// { dg-do assemble  }
// { dg-options "-O" }

class foo {
public:
  operator const char*() const { return a; }
  char *a;
};

class bar {
public:
  ~bar();
  void operator++(int);
  bool b() const;
protected:
  void* c() const;
};

class baz : public bar {
public:
  foo const &d() const { return *(foo *)bar::c(); }
};

extern int tst (const char *, const char *) throw();

void die(const foo& x)
{
  for (baz hi; hi.b(); hi++)
    if (tst (hi.d(), x) == 0)
      return;
}
