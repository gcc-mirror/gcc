// Test for implicitly deleted destructors.
// { dg-options "-std=c++0x" }
// { dg-prune-output "default definition would be ill-formed" }
// { dg-prune-output "within this context" }

class C
{
  void operator delete (void *); // { dg-error "private" }
public:
  virtual ~C();			// { dg-error "overriding" }
};

struct D: C { };		// { dg-error "deleted" }
D d;				// { dg-error "deleted" }

struct E
{
  ~E() = delete;		// { dg-error "declared here" }
};

struct F
{
  virtual ~F();			// { dg-error "overriding" }
};

struct G: E, F { };		// { dg-error "deleted" }
