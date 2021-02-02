// { dg-do assemble  }
// GROUPS passed visibility
class bottom
{
public:
  int b; 
};
class middle : private bottom // { dg-message "" } private
{
public:
  void foo () { b; }
};
class top : public middle
{
public:
  void bar () { b; }// { dg-error "" } .*
};
