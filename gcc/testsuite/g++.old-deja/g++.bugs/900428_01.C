// { dg-do assemble  }
// g++ 1.37.1 bug 900428_01

// g++ fails to issue error messages for cases where an incomplete type
// object must be evaluated if the value of such an evaluation is not
// actually used in the given context.

// In the case where such an object is volatile, it is obvious that this
// could be a problem, however I believe that errors should be issued
// for such cases regardless of whether or not such values are volatile
// because the abstract semantics seem to require the evaluation of such
// values whether they are volatile or not.

// [expr.static.cast/4, stmt.expr/1, expr.comma/1] show that expressions do
// not under go lvalue to rvalue decay, unless the value is actually used.
// This can be surprising when the object is volatile. We interpret a
// dereference of pointer to volatile to be a read.

// keywords: incomplete types, evaluation, volatile qualifier

int *ip_fn ();
int &ir_fn ();
volatile int *vip_fn ();
volatile int &vir_fn ();

void int_test (int i, int *p, volatile int *vp, int &r, volatile int &vr)
{
  int j;
  volatile int vj;
  
  *p;				// ok, no warning
  (void)*p;			// ok, no warning
  (void)(i ? j : *p);	        // ok, no warning
  (void)(i ? *p : j);	        // ok, no warning
  (void)((void)1, *p);	        // ok, no warning

  *vp;				// ok, no warning
  (void)*vp;			// ok, no warning
  (void)(i ? vj : *vp);	        // ok, no warning
  (void)(i ? *vp : vj);	        // ok, no warning
  (void)((void)1, *vp);         // ok, no warning

  r;				// ok, no warning
  (void)r;			// ok, no warning
  (void)(i ? j : r);	        // ok, no warning
  (void)(i ? r : j);	        // ok, no warning
  (void)((void)1, r);	        // ok, no warning

  vr;				// { dg-warning "" } reference not accessed
  (void)vr;			// { dg-warning "" } reference not accessed
  (void)(i ? vj : vr);	        // { dg-warning "" } reference not accessed
  (void)(i ? vr : vj);	        // { dg-warning "" } reference not accessed
  (void)((void)1, vr);          // { dg-warning "" } reference not accessed
  
  *ip_fn ();			// ok, no warning
  *vip_fn ();			// ok, no warning
  ir_fn ();			// ok, no warning
  vir_fn ();			// { dg-warning "" } reference not accessed
}

struct S;
S *sp_fn ();
S &sr_fn ();
volatile S *vsp_fn ();
volatile S &vsr_fn ();

void incomplete_test (int i, S *p, volatile S *vp, S &r, volatile S &vr)
{
  extern S j;
  extern volatile S vj;
  
  *p;				// ok, no warning
  (void)*p;			// ok, no warning
  (void)(i ? j : *p);	        // ok, no warning
  (void)(i ? *p : j);	        // ok, no warning
  (void)((void)1, *p);	        // ok, no warning

  *vp;				// { dg-warning "" } incomplete not accessed
  (void)*vp;			// { dg-warning "" } incomplete not accessed
  (void)(i ? vj : *vp);	        // { dg-warning "" } incomplete not accessed
  (void)(i ? *vp : vj);	        // { dg-warning "" } incomplete not accessed
  (void)((void)1, *vp);         // { dg-warning "" } incomplete not accessed

  r;				// ok, no warning
  (void)r;			// ok, no warning
  (void)(i ? j : r);	        // ok, no warning
  (void)(i ? r : j);	        // ok, no warning
  (void)((void)1, r);	        // ok, no warning

  vr;				// { dg-warning "" } reference not accessed
  (void)vr;			// { dg-warning "" } reference not accessed
  (void)(i ? vj : vr);	        // { dg-warning "" } reference not accessed
  (void)(i ? vr : vj);	        // { dg-warning "" } reference not accessed
  (void)((void)1, vr);          // { dg-warning "" } reference not accessed
  
  *sp_fn ();			// ok, no warning
  *vsp_fn ();			// { dg-warning "" } incomplete not accessed
  sr_fn ();			// ok, no warning
  vsr_fn ();			// { dg-warning "" } reference not accessed
}

struct T {int m;};
T *tp_fn ();
T &tr_fn ();
volatile T *vtp_fn ();
volatile T &vtr_fn ();

void complete_test (int i, T *p, volatile T *vp, T &r, volatile T &vr)
{
  T j;
  volatile T vj;
  
  *p;				// ok, no warning
  (void)*p;			// ok, no warning
  (void)(i ? j : *p);	        // ok, no warning
  (void)(i ? *p : j);	        // ok, no warning
  (void)((void)1, *p);	        // ok, no warning

  *vp;				// ok, no warning
  (void)*vp;			// ok, no warning
  (void)(i ? vj : *vp);	        // ok, no warning
  (void)(i ? *vp : vj);	        // ok, no warning
  (void)((void)1, *vp);         // ok, no warning

  r;				// ok, no warning
  (void)r;			// ok, no warning
  (void)(i ? j : r);	        // ok, no warning
  (void)(i ? r : j);	        // ok, no warning
  (void)((void)1, r);	        // ok, no warning

  vr;				// { dg-warning "" } reference not accessed
  (void)vr;			// { dg-warning "" } reference not accessed
  (void)(i ? vj : vr);	        // { dg-warning "" } reference not accessed
  (void)(i ? vr : vj);	        // { dg-warning "" } reference not accessed
  (void)((void)1, vr);          // { dg-warning "" } reference not accessed
  
  *tp_fn ();			// ok, no warning
  *vtp_fn ();			// ok, no warning
  tr_fn ();			// ok, no warning
  vtr_fn ();			// ok, no warning{ dg-warning "" } reference not accessed
}

void extern_test ()
{
  extern S es;
  extern volatile S ves;
  extern T et;
  extern volatile T vet;
  
  extern S &esr;
  extern volatile S &vesr;
  extern T &etr;
  extern volatile T &vetr;
  
  es;				// ok, no warning
  ves;				// { dg-warning "" } incomplete not accessed
  et;				// ok, no warning
  vet;				// ok, no warning
  
  esr;				// ok, no warning
  vesr;				// { dg-warning "" } incomplete not accessed
  etr;				// ok, no warning
  vetr;				// { dg-warning "" } reference not accessed
}
