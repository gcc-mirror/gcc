// Build don't link: 
// GROUPS passed visibility
class X {
      void g (int); // ERROR - private
public:
  void g (double);
};
	
class Y : public X { void f() { g (1); } }; // ERROR - private

