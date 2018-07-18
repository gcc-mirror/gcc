/* { dg-do compile } */

void deallocate(void *);
void *a;

struct C {
    virtual void m_fn1();
};

struct D {
    C *m_fn2() {
	if (a)
	  __builtin_abort();
    }	// { dg-warning "control reaches end of non-void function" }
};
D getd();

struct vec_int {
    int _M_start;
    ~vec_int() {
	if (_M_start)
	  deallocate(&_M_start);
    }
};
vec_int *b;

struct I {
    virtual void m_fn3();
};

void I::m_fn3() {
    if (a)
      getd().m_fn2()->m_fn1();
    b->~vec_int();
}

