// PR c++/88128 - Implement DR 330: Qualification conversions and pointers to
// arrays of pointers.

int *ar[4];
/* if at some level k the P2 is more cv-qualified than P1, then there
   must be a const at every single level (other than level zero) of P2
   up until k.  */
const int *(&arp)[4] = ar; // { dg-error "discards qualifiers" }
const int *const(&arp2)[4] = ar;
int *const(&arp3)[4] = ar;
int *(&arp4)[4] = ar;

const int *br[4];
const int *(&brp)[4] = br;
const int *const(&brp2)[4] = br;
int *const(&brp3)[4] = br; // { dg-error "discards qualifiers" }
int *(&brp4)[4] = br; // { dg-error "discards qualifiers" }

int *c[2][3];
int const *const (&cp1)[3] = *c;
int const *(&cp2)[3] = *c; // { dg-error "discards qualifiers" }
int *const (&cp3)[3] = *c;
int *(&cp4)[3] = *c;

double *const (*d)[3];
double const *const (&e)[3] = *d;

int *(*f)[3];
const int *const (&g)[3] = *f;
