/* Test -funsigned-bitfields works.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do run } */
/* { dg-options "-funsigned-bitfields -fsigned-char" } */

typedef char c;
typedef signed char sc;
typedef unsigned char uc;
typedef short s;
typedef signed short ss;
typedef unsigned short us;
typedef n;
typedef int i;
typedef signed int si;
typedef unsigned int ui;
typedef long l;
typedef signed long sl;
typedef unsigned long ul;
typedef long long ll;
typedef signed long long sll;
typedef unsigned long long ull;

typedef c ct;
typedef sc sct;
typedef uc uct;
typedef s st;
typedef ss sst;
typedef us ust;
typedef n nt;
typedef i it;
typedef si sit;
typedef ui uit;
typedef l lt;
typedef sl slt;
typedef ul ult;
typedef ll llt;
typedef sll sllt;
typedef ull ullt;

struct foo {
  char char0 : 1;
  c char1 : 1;
  ct char2 : 1;
  signed char schar0 : 1;
  sc schar1 : 1;
  sct schar2 : 1;
  unsigned char uchar0 : 1;
  uc uchar1 : 1;
  uct uchar2 : 1;
  short short0 : 1;
  s short1 : 1;
  st short2 : 1;
  signed short sshort0 : 1;
  ss sshort1 : 1;
  sst sshort2 : 1;
  unsigned short ushort0 : 1;
  us ushort1 : 1;
  ust ushort2 : 1;
  __attribute__((dummy)) int0 : 1; /* { dg-warning "attribute directive ignored" } */
  n int1 : 1;
  nt int2 : 1;
  int int3 : 1;
  i int4 : 1;
  it int5 : 1;
  signed int sint0 : 1;
  si sint1 : 1;
  sit sint2 : 1;
  unsigned int uint0 : 1;
  ui uint1 : 1;
  uit uint2 : 1;
  long long0 : 1;
  l long1 : 1;
  lt long2 : 1;
  signed long slong0 : 1;
  sl slong1 : 1;
  slt slong2 : 1;
  unsigned long ulong0 : 1;
  ul ulong1 : 1;
  ult ulong2 : 1;
  long long llong0 : 1;
  ll llong1 : 1;
  llt llong2 : 1;
  signed long long sllong0 : 1;
  sll sllong1 : 1;
  sllt sllong2 : 1;
  unsigned long long ullong0 : 1;
  ull ullong1 : 1;
  ullt ullong2 : 1;
};

struct foo x;

extern void abort (void);
extern void exit (int);
extern void *memset (void *, int, __SIZE_TYPE__);

int
main (void)
{
  memset (&x, (unsigned char)-1, sizeof(x));
  if (x.char0 != 1 || x.char1 != 1 || x.char2 != 1
      || x.schar0 != -1 || x.schar1 != -1 || x.schar2 != -1
      || x.uchar0 != 1 || x.uchar1 != 1 || x.uchar2 != 1
      || x.short0 != 1 || x.short1 != 1 || x.short2 != 1
      || x.sshort0 != -1 || x.sshort1 != -1 || x.sshort2 != -1
      || x.ushort0 != 1 || x.ushort1 != 1 || x.ushort2 != 1
      || x.int0 != 1 || x.int1 != 1 || x.int2 != 1
      || x.int3 != 1 || x.int4 != 1 || x.int5 != 1
      || x.sint0 != -1 || x.sint1 != -1 || x.sint2 != -1
      || x.uint0 != 1 || x.uint1 != 1 || x.uint2 != 1
      || x.long0 != 1 || x.long1 != 1 || x.long2 != 1
      || x.slong0 != -1 || x.slong1 != -1 || x.slong2 != -1
      || x.ulong0 != 1 || x.ulong1 != 1 || x.ulong2 != 1
      || x.llong0 != 1 || x.llong1 != 1 || x.llong2 != 1
      || x.sllong0 != -1 || x.sllong1 != -1 || x.sllong2 != -1
      || x.ullong0 != 1 || x.ullong1 != 1 || x.ullong2 != 1)
    abort ();
  exit (0);
}
