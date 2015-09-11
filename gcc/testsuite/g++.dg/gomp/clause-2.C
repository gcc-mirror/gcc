// { dg-do compile }

struct A { int a; };
struct B { B(); };
struct C { C(); C(const C&); };
struct D { D& operator=(const D&); };

class E { private: E(); public: E(int); };	// { dg-message "private" }
class F { private: F(const F&); public: F(); };	// { dg-message "private" }
class G { private: G& operator=(const G&); };	// { dg-message "private" }

void bar();
void foo()
{
  A a; B b; C c; D d; E e(0); F f; G g;

  #pragma omp parallel shared(a, b, c, d, e, f, g)
    bar();

  #pragma omp parallel private(a, b, c, d, f, g)
    bar();
  #pragma omp parallel private(e)		// { dg-error "context" }
    bar();

  #pragma omp parallel firstprivate(a, b, c, d, e, g)
    bar();
  #pragma omp parallel firstprivate(f)		// { dg-error "context" }
    bar();

  #pragma omp parallel sections lastprivate(a, b, d, c, f)
    { bar(); }
  #pragma omp parallel sections lastprivate(e)	// { dg-error "context" }
    { bar(); }
  #pragma omp parallel sections lastprivate(g)	// { dg-error "context" }
    { bar(); }
  #pragma omp parallel sections firstprivate(e) lastprivate(e)
    { bar(); }
}
