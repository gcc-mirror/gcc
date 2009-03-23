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
