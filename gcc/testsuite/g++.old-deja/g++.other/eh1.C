// { dg-do assemble  }

class C2 {
public:
  ~C2();
  C2 a() const;
};
class C3 {
public:
  C3(const C2 &c);
};
class C4
{
public:
  C3 *foo(bool b, const C2 &c);
  C2 d() const;
};
C3 *C4::foo(bool b, const C2 &c)
{
  return new C3(b ? d().a() : c);
}
