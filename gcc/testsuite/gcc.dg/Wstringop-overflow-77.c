/* PR middle-end/102453 - buffer overflow by atomic built-ins not diagnosed
   Verify that out-of-bounds accesses by atomic functions are diagnosed with
   optimization disabled.
   { dg-do compile }
   { dg-options "-O0 -Wall -ftrack-macro-expansion=0" }  */

#ifndef __cplusplus
#  define bool _Bool
#endif

#define add_fetch(p, q)    __atomic_add_fetch (p, q, 0)
#define sub_fetch(p, q)    __atomic_sub_fetch (p, q, 0)
#define and_fetch(p, q)    __atomic_and_fetch (p, q, 0)
#define or_fetch(p, q)     __atomic_or_fetch (p, q, 0)
#define xor_fetch(p, q)    __atomic_xor_fetch (p, q, 0)
#define nand_fetch(p, q)   __atomic_nand_fetch (p, q, 0)
#define exchange(p, q, r)  __atomic_exchange (p, q, r, 0)
#define exchange_n(p, n)   __atomic_exchange_n (p, n, 0)
#define cmpxchg(p, q, r)   __atomic_compare_exchange (p, q, r, 0, 0, 0)

typedef __SIZE_TYPE__ size_t;

void sink (void*, ...);
#define sink(...) sink (0, __VA_ARGS__)

extern _Bool eb;
extern char ec;
extern short int esi;
extern int ei;
extern long int eli;
extern long long int elli;

extern const _Bool ecb;
extern const char ecc;
extern const short int ecsi;
extern const int eci;
extern const long int ecli;
extern const long long int eclli;

extern _Atomic _Bool eab;
extern _Atomic char eac;
extern _Atomic short int easi;
extern _Atomic int eai;
extern _Atomic long int eali;
extern _Atomic long long int ealli;

extern _Atomic const _Bool eacb;
extern _Atomic const char eacc;
extern _Atomic const short int eacsi;
extern _Atomic const int eaci;
extern _Atomic const long int eacli;
extern _Atomic const long long int eaclli;


void nowarn_atomic_add_fetch (void)
{
  add_fetch (&eac, ecc);
  add_fetch (&easi, esi);
  add_fetch (&eai, ei);
  add_fetch (&eali, eli);
  add_fetch (&ealli, elli);
}


void warn_atomic_add_fetch (void)
{
  _Atomic char *pc = &eac + 1;
  add_fetch (pc, ecc);                  // { dg-warning "-Wstringop-overflow" }

  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  add_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 2);
  add_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  add_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  add_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  add_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }

  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  add_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  add_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  add_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  add_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&ealli + 1);
  add_fetch (plli, eali);               // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  add_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
}


void nowarn_atomic_sub_fetch (void)
{
  _Atomic char *pc = &eac;
  sub_fetch (pc, ecc);

  _Atomic short *psi = &easi;
  sub_fetch (psi, esi);

  _Atomic int *pi = &eai;
  sub_fetch (pi, ei);

  _Atomic long *pli = &eali;
  sub_fetch (pli, eli);

  _Atomic long long *plli = &ealli;
  sub_fetch (plli, elli);
}


void warn_atomic_sub_fetch (void)
{
  _Atomic char *pc = &eac + 1;
  sub_fetch (pc, ecc);                  // { dg-warning "-Wstringop-overflow" }

  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  sub_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 2);
  sub_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  sub_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  sub_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  sub_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }

  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  sub_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  sub_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  sub_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  sub_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&ealli + 1);
  sub_fetch (plli, eali);               // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  sub_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
}


void nowarn_atomic_and_fetch (void)
{
  _Atomic char *pc = &eac;
  and_fetch (pc, ecc);

  _Atomic short *psi = &easi;
  and_fetch (psi, esi);

  _Atomic int *pi = &eai;
  and_fetch (pi, ei);

  _Atomic long *pli = &eali;
  and_fetch (pli, eli);

  _Atomic long long *plli = &ealli;
  and_fetch (plli, elli);
}


void warn_atomic_and_fetch (void)
{
  _Atomic char *pc = &eac + 1;
  and_fetch (pc, ecc);                  // { dg-warning "-Wstringop-overflow" }

  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  and_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 2);
  and_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  and_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  and_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  and_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }

  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  and_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  and_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  and_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  and_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&ealli + 1);
  and_fetch (plli, eali);               // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  and_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
}


void nowarn_atomic_or_fetch (void)
{
  _Atomic char *pc = &eac;
  or_fetch (pc, ecc);

  _Atomic short *psi = &easi;
  or_fetch (psi, esi);

  _Atomic int *pi = &eai;
  or_fetch (pi, ei);

  _Atomic long *pli = &eali;
  or_fetch (pli, eli);

  _Atomic long long *plli = &ealli;
  or_fetch (plli, elli);
}


void warn_atomic_or_fetch (void)
{
  _Atomic char *pc = &eac + 1;
  or_fetch (pc, ecc);                   // { dg-warning "-Wstringop-overflow" }

  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  or_fetch (psi, esi);                  // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 2);
  or_fetch (psi, esi);                  // { dg-warning "-Wstringop-overflow" }

  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  or_fetch (pi, ei);                    // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  or_fetch (pi, ei);                    // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  or_fetch (pi, ei);                    // { dg-warning "-Wstringop-overflow" }

  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  or_fetch (pli, eli);                  // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  or_fetch (pli, eli);                  // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  or_fetch (pli, eli);                  // { dg-warning "-Wstringop-overflow" }

  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  or_fetch (plli, elli);                // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&ealli + 1);
  or_fetch (plli, eali);                // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  or_fetch (plli, elli);                // { dg-warning "-Wstringop-overflow" }
}


void nowarn_atomic_xor_fetch (void)
{
  _Atomic char *pc = &eac;
  xor_fetch (pc, ecc);

  _Atomic short *psi = &easi;
  xor_fetch (psi, esi);

  _Atomic int *pi = &eai;
  xor_fetch (pi, ei);

  _Atomic long *pli = &eali;
  xor_fetch (pli, eli);

  _Atomic long long *plli = &ealli;
  xor_fetch (plli, elli);
}


void warn_atomic_xor_fetch (void)
{
  _Atomic char *pc = &eac + 1;
  xor_fetch (pc, ecc);                  // { dg-warning "-Wstringop-overflow" }

  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  xor_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 1);
  xor_fetch (psi, esi);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  xor_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  xor_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  xor_fetch (pi, ei);                   // { dg-warning "-Wstringop-overflow" }

  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  xor_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  xor_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  xor_fetch (pli, eli);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  xor_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&eali + 1);
  xor_fetch (plli, eali);               // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  xor_fetch (plli, elli);               // { dg-warning "-Wstringop-overflow" }
}


void nowarn_atomic_nand_fetch (void)
{
  _Atomic char *pc = &eac;
  nand_fetch (pc, ecc);

  _Atomic short *psi = &easi;
  nand_fetch (psi, esi);

  _Atomic int *pi = &eai;
  nand_fetch (pi, ei);

  _Atomic long *pli = &eali;
  nand_fetch (pli, eli);

  _Atomic long long *plli = &ealli;
  nand_fetch (plli, elli);
}


void warn_atomic_nand_fetch (void)
{
  _Atomic char *pc = &eac + 1;
  nand_fetch (pc, ecc);                 // { dg-warning "-Wstringop-overflow" }

  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  nand_fetch (psi, esi);                // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 1);
  nand_fetch (psi, esi);                // { dg-warning "-Wstringop-overflow" }

  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  nand_fetch (pi, ei);                  // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  nand_fetch (pi, ei);                  // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  nand_fetch (pi, ei);                  // { dg-warning "-Wstringop-overflow" }

  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  nand_fetch (pli, eli);                // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  nand_fetch (pli, eli);                // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  nand_fetch (pli, eli);                // { dg-warning "-Wstringop-overflow" }

  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  nand_fetch (plli, elli);              // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&eai + 1);
  nand_fetch (plli, eali);              // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  nand_fetch (plli, elli);              // { dg-warning "-Wstringop-overflow" }
}


void nowarn_atomic_exchange (void)
{
  char rc;
  _Atomic char *pc = &eac;
  exchange (pc, &ecc, &rc);

  short rsi;
  _Atomic short *psi = &easi;
  exchange (psi, &esi, &rsi);

  int ri;
  _Atomic int *pi = &eai;
  exchange (pi, &ei, &ri);

  long rli;
  _Atomic long *pli = &eali;
  exchange (pli, &eli, &rli);

  long long rlli;
  _Atomic long long *plli = &ealli;
  exchange (plli, &elli, &rlli);

  sink (&rc, &rsi, &ri, &rli, &rlli);
}

void warn_atomic_exchange (void)
{
  char rc;
  _Atomic char *pc = &eac + 1;
  exchange (pc, &ecc, &rc);             // { dg-warning "-Wstringop-overflow" }

  short rsi[2];
  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  exchange (psi, &ecsi, rsi);           // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 2);
  exchange (psi, &ecsi, rsi + 1);       // { dg-warning "-Wstringop-overflow" }

  int ri[3];
  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  exchange (pi, &eci, ri);              // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  exchange (pi, &eci, ri + 1);          // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  exchange (pi, &eci, ri + 2);          // { dg-warning "-Wstringop-overflow" }

  long rli[3];
  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  exchange (pli, &ecli, rli);           // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  exchange (pli, &ecli, rli + 1);       // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  exchange (pli, &ecli, rli + 2);       // { dg-warning "-Wstringop-overflow" }

  long long rlli[3];
  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  exchange (plli, &eclli, rlli);        // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&ealli + 1);
  exchange (plli, &eclli, rlli + 1);    // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  exchange (plli, &eclli, rlli + 2);    // { dg-warning "-Wstringop-overflow" }

  sink (&rc, rsi, ri, rli, rlli);
}


void nowarn_atomic_exchange_n (_Atomic unsigned char *pauc,
			       _Atomic unsigned short *pausi,
			       _Atomic unsigned int *paui,
			       _Atomic unsigned long *pauli,
			       _Atomic unsigned long long *paulli)
{
  char rc = exchange_n (&eac, ecc);
  short rsi = exchange_n (&easi, esi);
  int ri = exchange_n (&eai, ei);
  long rli = exchange_n (&eali, eli);
  long long rlli = exchange_n (&ealli, elli);

  sink (rc, rsi, ri, rli, rlli);

  char ruc = exchange_n (pauc, ecc);
  short rusi = exchange_n (pausi, esi);
  int rui = exchange_n (paui, ei);
  long ruli = exchange_n (pauli, eli);
  long long rulli = exchange_n (paulli, elli);

  sink (ruc, rusi, rui, ruli, rulli);
}


void warn_atomic_exchange_n (void)
{
  _Atomic char *pc = &eac + 1;
  char rc = exchange_n (pc, ecc);       // { dg-warning "-Wstringop-overflow" }

  short rsi[2];
  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  rsi[0] = exchange_n (psi, ecsi);      // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 2);
  rsi[1] = exchange_n (psi, ecsi);      // { dg-warning "-Wstringop-overflow" }

  int ri[3];
  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  ri[0] = exchange_n (pi, eci);         // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  ri[1] = exchange_n (pi, eci);         // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  ri[2] = exchange_n (pi, eci);         // { dg-warning "-Wstringop-overflow" }

  long rli[3];
  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  rli[0] = exchange_n (pli, ecli);      // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  rli[1] = exchange_n (pli, ecli);      // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  rli[2] = exchange_n (pli, ecli);      // { dg-warning "-Wstringop-overflow" }

  long long rlli[3];
  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  rlli[0] = exchange_n (plli, eclli);   // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&ealli + 1);
  rlli[1] = exchange_n (plli, eclli);   // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  rlli[2] = exchange_n (plli, eclli);   // { dg-warning "-Wstringop-overflow" }

  sink (&rc, rsi, ri, rli, rlli);
}


void warn_atomic_compare_exchange (void)
{
  _Atomic char *pc = &eac + 1;
  cmpxchg (pc, &ec, &ecc);              // { dg-warning "-Wstringop-overflow" }

  _Atomic short *psi = (_Atomic short*)((char*)&easi + 1);
  cmpxchg (psi, &esi, &ecsi);           // { dg-warning "-Wstringop-overflow" }
  psi = (_Atomic short*)((char*)&easi + 2);
  cmpxchg (psi, &esi, &ecsi);           // { dg-warning "-Wstringop-overflow" }

  _Atomic int *pi = (_Atomic int*)((char*)&eai + 1);
  cmpxchg (pi, &ei, &eci);              // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + 2);
  cmpxchg (pi, &ei, &eci);              // { dg-warning "-Wstringop-overflow" }
  pi = (_Atomic int*)((char*)&eai + sizeof eai);
  cmpxchg (pi, &ei, &eci);              // { dg-warning "-Wstringop-overflow" }

  _Atomic long *pli = (_Atomic long*)((char*)&eali + 1);
  cmpxchg (pli, &eli, &ecli);           // { dg-warning "-Wstringop-overflow" }
  pli = (_Atomic long*)((char*)&eali + 1);
  cmpxchg (pli, &eli, &ecli);           // { dg-warning "-Wstringop-overflow" }
  pli = &eali + 1;
  cmpxchg (pli, &eli, &ecli);           // { dg-warning "-Wstringop-overflow" }

  _Atomic long long *plli = (_Atomic long long*)((char*)&ealli + 1);
  cmpxchg (plli, &elli, &eclli);        // { dg-warning "-Wstringop-overflow" }
  plli = (_Atomic long long*)((char*)&ealli + 1);
  cmpxchg (plli, &elli, &eclli);        // { dg-warning "-Wstringop-overflow" }
  plli = &ealli + 1;
  cmpxchg (plli, &elli, &eclli);        // { dg-warning "-Wstringop-overflow" }
}
