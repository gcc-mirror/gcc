// { dg-do assemble  }

struct
Foo
{
public:
  typedef void* (*copier_fn)(void const*);
  void foo() const;
  void bar(char const*, void const*) const;
private:
  struct
  Bar
  {
    char const* key;
    void const* item;
  };
};

void
Foo::foo() const
{
  Bar* cp = 0;
  copier_fn copyfn = 0;

  bar(cp->key, cp->item);
  bar(cp->key, (copyfn) ? (*copyfn)(cp) : 0);
  bar(cp->key, (copyfn) ? (*copyfn)(0) : 0);

  bar(cp->key, (copyfn) ? (*copyfn)(0) : cp->item);
  bar(cp->key, (copyfn) ? (*copyfn)(cp) : cp->item);
}
