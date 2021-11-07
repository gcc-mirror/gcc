/* PR middle-end/102453 - buffer overflow by atomic built-ins not diagnosed
   Verify that out-of-bounds accesses by atomic functions are diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" }  */

#ifndef __cplusplus
#  define bool _Bool
#endif

#define load        __atomic_load
#define store       __atomic_store
#define add_fetch   __atomic_add_fetch
#define sub_fetch   __atomic_sub_fetch
#define and_fetch   __atomic_and_fetch
#define or_fetch    __atomic_or_fetch
#define xor_fetch   __atomic_xor_fetch
#define nand_fetch  __atomic_nand_fetch

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


void nowarn_atomic_load (void)
{
  load (&eacb, &eb, 0);
  load (&eacc, &ec, 0);
  load (&eacsi, &esi, 0);
  load (&eaci, &ei, 0);
  load (&eacli, &eli, 0);
  load (&eaclli, &elli, 0);
}


void warn_atomic_load_note (void)
{
  int i;                            // { dg-message "'i'" }

  int *pi = (int*)((char*)&i + 1);
  load (&eaci, pi, 0);              // { dg-warning "-Warray-bounds" }
  sink (&i);

  pi = (int*)((char*)&i + 2);
  load (&eaci, pi, 0);              // { dg-warning "-Warray-bounds" }
  sink (&i);

  pi = &i + 1;
  load (&eaci, pi, 0);              // { dg-warning "-Warray-bounds" }
  sink (&i);
}


void warn_atomic_load (void)
{
  bool *pb = &eb + 1;
  load (&eacb, pb, 0);              // { dg-warning "-Warray-bounds" }

  char *pc = &ec + 1;
  load (&eacc, pc, 0);              // { dg-warning "-Warray-bounds" }

  short *psi = (short*)((char*)&esi + 1);
  load (&eacsi, psi, 0);            // { dg-warning "-Warray-bounds" }
  psi = (short*)((char*)&esi + 2);
  load (&eacsi, psi, 0);            // { dg-warning "-Warray-bounds" }

  int *pi = (int*)((char*)&ei + 1);
  load (&eaci, pi, 0);              // { dg-warning "-Warray-bounds" }
  pi = (int*)((char*)&ei + 2);
  load (&eaci, pi, 0);              // { dg-warning "-Warray-bounds" }
  pi = (int*)((char*)&ei + sizeof ei);
  load (&eaci, pi, 0);              // { dg-warning "-Warray-bounds" }

  long *pli = (long*)((char*)&eli + 1);
  load (&eacli, pli, 0);            // { dg-warning "-Warray-bounds" }
  pli = (long*)((char*)&eli + 1);
  load (&eacli, pli, 0);            // { dg-warning "-Warray-bounds" }
  pli = &eli + 1;
  load (&eacli, pli, 0);            // { dg-warning "-Warray-bounds" }

  long long *plli = (long long*)((char*)&elli + 1);
  load (&eaclli, plli, 0);          // { dg-warning "-Warray-bounds" }
  plli = (long long*)((char*)&elli + 1);
  load (&eaclli, plli, 0);          // { dg-warning "-Warray-bounds" }
  plli = &elli + 1;
  load (&eaclli, plli, 0);          // { dg-warning "-Warray-bounds" }
}


void warn_atomic_store (void)
{
  const bool *pb = &eb + 1;
  store (&eab, pb, 0);              // { dg-warning "-Warray-bounds" }

  const char *pc = &ec + 1;
  store (&eac, pc, 0);              // { dg-warning "-Warray-bounds" }

  const short *psi = (const short*)((const char*)&ecsi + 1);
  store (&easi, psi, 0);            // { dg-warning "-Warray-bounds" }
  psi = (const short*)((const char*)&esi + 2);
  store (&easi, psi, 0);            // { dg-warning "-Warray-bounds" }

  const int *pi = (const int*)((const char*)&eci + 1);
  store (&eai, pi, 0);              // { dg-warning "-Warray-bounds" }
  pi = (const int*)((const char*)&ei + 2);
  store (&eai, pi, 0);              // { dg-warning "-Warray-bounds" }
  pi = (const int*)((const char*)&ei + sizeof ei);
  store (&eai, pi, 0);              // { dg-warning "-Warray-bounds" }

  const long *pli = (const long*)((const char*)&eli + 1);
  store (&eali, pli, 0);            // { dg-warning "-Warray-bounds" }
  pli = (const long*)((const char*)&eli + sizeof (eli));
  store (&eali, pli, 0);            // { dg-warning "-Warray-bounds" }

  const long long *plli = (const long long*)((const char*)&elli + 1);
  store (&ealli, plli, 0);          // { dg-warning "-Warray-bounds" }
  plli = (const long long*)((const char*)&elli + sizeof elli);
  store (&ealli, plli, 0);          // { dg-warning "-Warray-bounds" }
}
