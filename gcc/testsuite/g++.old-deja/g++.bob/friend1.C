// { dg-do assemble  }
class C {
public:
  static friend int f(); // { dg-error "" } 
};
