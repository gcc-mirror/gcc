// { dg-do assemble  }
// GROUPS passed visibility
class X {
      void g (int); // { dg-error "" } private
public:
  void g (double);
};
	
class Y : public X { void f() { g (1); } }; // { dg-error "" } private

