// PR c++/63707
// { dg-do compile { target c++11 } }

struct Child
{
  Child (int);
  ~Child ();
  Child (const Child &) = delete;
};

struct Parent
{
  Parent () : children {{5}, {7}} {}

  Child children[2];
};
