// PR c++/51553
// { dg-do compile { target c++11 } }

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

X c = { Z() };  // { dg-error "" }
X cc = Z();	// { dg-error "" }

X d{ Z() };
X dd( Z() );
