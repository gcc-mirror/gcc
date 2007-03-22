// { dg-do run  }
// PRMS Id: 6905

class Parent {
public:
  void DoSomething() { return; }
  int i;
};

class Child : public Parent {
public:
};

class User {
public:
  void DoAnyThing(void (Parent::*)(void)) { return; }
  void DoAThing(void (Child::*)(void)) { return; }
  void DoAThing(int Child::*) { return; }
};


int main()
{
  User a;

  a.DoAnyThing(&Child::DoSomething);
  a.DoAThing(&Child::DoSomething);
  a.DoAThing(&Parent::DoSomething);
  a.DoAThing(&Parent::i);
}
