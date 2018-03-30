// Test for implicitly deleted destructors.
// { dg-do compile { target c++11 } }
// { dg-prune-output "default definition would be ill-formed" }
// { dg-prune-output "within this context" }

class C
{
  void operator delete (void *); // { dg-message "private" }
public:
  virtual ~C();			// { dg-message "overridden" }
};

struct D: C { };		// { dg-error "deleted" }
D d;				// { dg-error "deleted" }

struct E
{
  ~E() = delete;		// { dg-message "declared here" }
};

struct F
{
  virtual ~F();			// { dg-message "overridden" }
};

struct G: E, F { };		// { dg-error "deleted" }
