// { dg-do assemble  }
// { dg-additional-options "-Wno-register" }

class foo_a {
  protected:
        double func(int xi) {return 0.0;}
  public:
        foo_a() {}
};

class foo_b {
  public:
        foo_b(int);
        foo_b();
        ~foo_b();
        foo_b(const foo_b&);
        double& operator()(int);
        foo_b& operator=(foo_b&);
        void bar_a(int);
};

foo_b& operator*(foo_b&, foo_b&);
foo_b& operator*(double, foo_b&);

template <class TP>
class foo_c {
        typedef double (TP::* Tmatf)(int);
        int m;
        Tmatf* a;
        void foo_cinst (int mm);
  public:
        foo_c(int mm);
        foo_c() {m = 0; a = 0;}
        ~foo_c() {delete a;}
        double (TP::*& operator()(int i))(int) {return a[i];}
        foo_b& bug_func(int);
};

template <class TP>
foo_b& foo_c<TP>::bug_func(int x) {
        static foo_b retval(m);
        retval.bar_a(m);
        for (register int i = 0; i < m; i++)
	  retval(i) = (*(operator()(i)))(x);		// { dg-error "invalid use of unary '\\\*'" } 
        return retval;
}

template <class TP>
class foo_d {
  protected:
        foo_c<TP> bar_b;
  public:
        foo_d() {}
        virtual ~foo_d() {}
        virtual void setfoo_c();
};

class foo_e : public foo_a, public foo_d<foo_a> {
  public:
        foo_e();
        ~foo_e() {}
        void setfoo_c();
};

void foo_e::setfoo_c() {
        bar_b(0) = func;				// { dg-error "" } 
}

template class foo_c<foo_a>;
