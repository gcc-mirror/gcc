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
struct s;
extern struct s es, *ps;
extern volatile struct s evs, *pvs;

void pv_test ()
{
  *pv;			// ERROR - , XFAIL *-*-*
  (i ? *pv : *pv);	// ERROR - , XFAIL *-*-*
  *pv, *pv;		// ERROR - , XFAIL *-*-*

  *pvv;			// ERROR - , XFAIL *-*-*
  (i ? *pvv : *pvv);	// ERROR - , XFAIL *-*-*
  *pvv, *pvv;		// ERROR - , XFAIL *-*-*

  es;			// ERROR - , XFAIL *-*-*
  (i ? es : es);	// ERROR - , XFAIL *-*-*
  es, es;		// ERROR - , XFAIL *-*-*

  evs;			// ERROR - , XFAIL *-*-*
  (i ? evs : evs);	// ERROR - , XFAIL *-*-*
  evs, evs;		// ERROR - , XFAIL *-*-*

  *ps;			// ERROR - , XFAIL *-*-*
  (i ? *ps : *ps);	// ERROR - , XFAIL *-*-*
  *ps, *ps;		// ERROR - , XFAIL *-*-*

  *pvs;			// ERROR - , XFAIL *-*-*
  (i ? *pvs : *pvs);	// ERROR - , XFAIL *-*-*
  *pvs, *pvs;		// ERROR - , XFAIL *-*-*
}

int main () { return 0; }
