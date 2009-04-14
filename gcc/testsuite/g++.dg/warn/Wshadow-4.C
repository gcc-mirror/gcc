// PR c++/39526
// { dg-options "-Wshadow" }

class INetURLObject
{
public:
    INetURLObject(int i);
    int GetMainURL() const;
};

int foo(int infoo)		// { dg-warning "shadowed declaration" }
{
  int outfoo( INetURLObject( infoo ).GetMainURL()); // { dg-bogus "shadows" }
  extern void f(int infoo);
  struct A
  {
    void f(int infoo) { }	// { dg-warning "shadows a parameter" }
  };
  return outfoo;
}

// PR c++/39763
int foo2(void)
{
    int infoo = 0;		// { dg-warning "shadowed declaration" }
    int outfoo( INetURLObject( infoo ).GetMainURL()); // { dg-bogus "shadows" }
    struct A
    {
      void f(int infoo) { }	// { dg-warning "shadows a previous local" }
    };
    return outfoo;
}
