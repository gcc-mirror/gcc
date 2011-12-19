// PR c++/51553
// { dg-options -std=c++0x }

struct X
{
  X();
};

struct Y
{
  operator X() const;
};

struct Z
{
  explicit operator X() const;
};

X a = { Y() };
X aa = Y();

X b{ Y() };
X bb(Y());

X c = { Z() };  // { dg-error "" "" { xfail *-*-* } }
X cc = Z();	// { dg-error "" }

X d{ Z() };
X dd( Z() );
