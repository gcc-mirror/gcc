// g++ 1.37.1 bug 900428_01

// g++ fails to issue error messages for cases where an incomplete type
// object must be evaluated if the value of such an evaluation is not
// actually used in the given context.

// In the case where such an object is volatile, it is obvious that this
// could be a problem, however I believe that errors should be issued
// for such cases regardless of whether or not such values are volatile
// because the abstract semantics seem to require the evaluation of such
// values whether they are volatile or not.

// keywords: incomplete types, evaluation, volatile qualifier
// Build don't link: 

int i;

void *pv;
volatile void *pvv;
struct s;               // ERROR - forward declaration
extern struct s es, *ps;  // ERROR - defined here
extern volatile struct s evs, *pvs; // ERROR - defined here

void pv_test ()
{
  *pv;			// ERROR - invalid void
  (i ? *pv : *pv);	// ERROR - invalid void
  *pv, *pv;		// ERROR - invalid void

  *pvv;			// ERROR - invalid void
  (i ? *pvv : *pvv);	// ERROR - invalid void
  *pvv, *pvv;		// ERROR - invalid void

  es;			// ERROR - incomplete
  (i ? es : es);	// ERROR - undefined type
  es, es;		// ERROR - incomplete

  evs;			// ERROR - incomplete
  (i ? evs : evs);	// ERROR - undefined type
  evs, evs;		// ERROR - incomplete

  *ps;			// ERROR - undefined type
  (i ? *ps : *ps);	// ERROR - undefined type
  *ps, *ps;		// ERROR - undefined type

  *pvs;			// ERROR - undefined type
  (i ? *pvs : *pvs);	// ERROR - undefined type
  *pvs, *pvs;		// ERROR - undefined type
}

int main () { return 0; }
