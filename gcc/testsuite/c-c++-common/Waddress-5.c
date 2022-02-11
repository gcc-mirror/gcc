/* PR c/33925 - missing -Waddress with the address of an inline function
   { dg-do compile }
   { dg-options "-Wall" }
   { dg-require-weak "" } */

extern inline int eifn (void);
extern inline int eifn_def (void) { return 0; }

static inline int sifn (void);
static inline int sifn_def (void) { return 0; }

inline int ifn (void);
inline int ifn_def (void) { return 0; }

extern __attribute__ ((weak)) int ewfn (void);
extern __attribute__ ((weak)) int ewfn_def (void) { return 0;  }

__attribute__ ((weak)) int wfn (void);
__attribute__ ((weak)) int wfn_def (void) { return 0; }

static __attribute__((weakref ("ewfn"))) int swrfn (void);

void test_function_eqz (int *p)
{
  *p++ = eifn == 0;                     // { dg-warning "-Waddress" }
  *p++ = eifn_def == 0;                 // { dg-warning "-Waddress" }
  *p++ = sifn == 0;                     // { dg-warning "-Waddress" }
  *p++ = sifn_def == 0;                 // { dg-warning "-Waddress" }
  *p++ = ifn == 0;                      // { dg-warning "-Waddress" }
  *p++ = ifn_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = ewfn == 0;
  *p++ = ewfn_def == 0;                 // { dg-warning "-Waddress" }
  *p++ = wfn == 0;
  *p++ = wfn_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = swrfn == 0;
}


int test_function_if (int i)
{
  if (eifn)                             // { dg-warning "-Waddress" }
    i++;
  if (eifn_def)                         // { dg-warning "-Waddress" }
    i++;
  if (sifn)                             // { dg-warning "-Waddress" }
    i++;
  if (sifn_def)                         // { dg-warning "-Waddress" }
    i++;
  if (ifn)                              // { dg-warning "-Waddress" }
    i++;
  if (ifn_def)                          // { dg-warning "-Waddress" }
    i++;
  if (ewfn)
    i++;
  if (ewfn_def)                         // { dg-warning "-Waddress" }
    i++;
  if (wfn)
    i++;
  if(wfn_def)                           // { dg-warning "-Waddress" }
    i++;
  if (swrfn)
    i++;
  return i;
}


extern int ei;
extern int ei_def = 1;

static int si;
static int si_def = 1;

int i;
int i_def = 1;

extern __attribute__ ((weak)) int ewi;   // declaration (may be null)
extern __attribute__ ((weak)) int ewi_def = 1;

__attribute__ ((weak)) int wi;          // definition (cannot be bull)
__attribute__ ((weak)) int wi_def = 1;

static __attribute__((weakref ("ewi"))) int swri;

void test_scalar (int *p)
{
  *p++ = &ei == 0;                      // { dg-warning "-Waddress" }
  *p++ = &ei_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = &si == 0;                      // { dg-warning "-Waddress" }
  *p++ = &si_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = &i == 0;                       // { dg-warning "-Waddress" }
  *p++ = &i_def == 0;                   // { dg-warning "-Waddress" }
  *p++ = &ewi == 0;
  *p++ = &ewi_def == 0;                 // { dg-warning "-Waddress" }
  *p++ = &wi == 0;                      // { dg-warning "-Waddress" }
  *p++ = &wi_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = &swri == 0;
}


extern int eia[];
extern int eia_def[] = { 1 };

static int sia[1];
static int sia_def[1] = { 1 };

int ia[1];
int ia_def[] = { 1 };

extern __attribute__ ((weak)) int ewia[];
extern __attribute__ ((weak)) int ewia_def[] = { 1 };

__attribute__ ((weak)) int wia[1];      // definition (cannot be null)
__attribute__ ((weak)) int wia_def[] = { 1 };

static __attribute__((weakref ("ewia"))) int swria[1];

void test_array (int *p)
{
  *p++ = eia == 0;                      // { dg-warning "-Waddress" }
  *p++ = eia_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = sia == 0;                      // { dg-warning "-Waddress" }
  *p++ = sia_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = ia == 0;                       // { dg-warning "-Waddress" }
  *p++ = ia_def == 0;                   // { dg-warning "-Waddress" }
  *p++ = ewia == 0;
  *p++ = ewia_def == 0;                 // { dg-warning "-Waddress" }
  *p++ = wia == 0;                      // { dg-warning "-Waddress" }
  *p++ = wia_def == 0;                  // { dg-warning "-Waddress" }
  *p++ = swria == 0;
}

/* { dg-prune-output "never defined" }
   { dg-prune-output "initialized and declared 'extern'" } */
