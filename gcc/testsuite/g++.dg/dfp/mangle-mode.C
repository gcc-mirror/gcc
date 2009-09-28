// { dg-do compile }

// Check that the compiler mangles types defined with decimal float
// modes according to the vendor-neutral C++ ABI.

typedef float _Decimal32 __attribute__((mode(SD)));
typedef float _Decimal64 __attribute__((mode(DD)));
typedef float _Decimal128 __attribute__((mode(TD)));

extern void foo32 (_Decimal32 a, _Decimal32 &b, _Decimal32 *c);
extern void foo64 (_Decimal64 *a, _Decimal64 b, _Decimal64 &c);
extern void foo128 (_Decimal128 &a, _Decimal128 *b, _Decimal128 c);

void
bar32 (void)
{
  _Decimal32 x, y, z;
  foo32 (x, y, &z);
}

void
bar64 (void)
{
  _Decimal64 x, y, z;
  foo64 (&x, y, z);
}

void
bar128 (void)
{
  _Decimal128 x, y, z;
  foo128 (x, &y, z);
}

// { dg-final { scan-assembler "Z5foo32DfRDfPDf" } }
// { dg-final { scan-assembler "Z5foo64PDdDdRDd" } }
// { dg-final { scan-assembler "Z6foo128RDePDeDe" } }
