// Build don't link:

struct S0 { };
struct S1 : virtual public S0 { };
struct S2 : virtual public S0 { };

struct S3 : public S1, public S2, virtual public S0
{
};

void f(const S0*) {}

void g()
{
  f(static_cast<S3*>(0));
}
