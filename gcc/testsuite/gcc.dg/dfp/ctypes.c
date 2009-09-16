/* { dg-do compile } */

/* N1150 3: Decimal floating types.
   C99 6.7.2: Type specifiers  */

/* Test for the existence of the types.  */
_Decimal32 sd1;
_Decimal64 dd2;
_Decimal128 td3;

#define ARRAY_SIZE      7

static _Decimal32 d32[ARRAY_SIZE];
static _Decimal64 d64[ARRAY_SIZE];
static _Decimal128 d128[ARRAY_SIZE];

extern _Decimal32 ext_d32[ARRAY_SIZE];
extern _Decimal64 ext_d64[ARRAY_SIZE];
extern _Decimal128 ext_d128[ARRAY_SIZE];

/* Test sizes for these types.  */
int ssize[sizeof (_Decimal32) == 4 ? 1 : -1];
int dsize[sizeof (_Decimal64) == 8 ? 1 : -1];
int tsize[sizeof (_Decimal128) == 16 ? 1 : -1];

int salign = __alignof (_Decimal32);
int dalign = __alignof (_Decimal64);
int talign = __alignof (_Decimal128);

/* sizeof operator applied on an array of DFP types is n times the
   size of a single variable of this type.  */

int d32_array_size [sizeof(d32) == ARRAY_SIZE * sizeof(sd1) ? 1 : -1];
int d64_array_size [sizeof(d64) == ARRAY_SIZE * sizeof(dd2) ? 1 : -1];
int d128_array_size [sizeof(d128) == ARRAY_SIZE * sizeof(td3)? 1 : -1];

/* Likewise for extern qualified arrays.  */

int ext_d32_array_size [sizeof(ext_d32) == ARRAY_SIZE * sizeof(sd1) ? 1 : -1];
int ext_d64_array_size [sizeof(ext_d64) == ARRAY_SIZE * sizeof(dd2) ? 1 : -1];
int ext_d128_array_size [sizeof(ext_d128) == ARRAY_SIZE * sizeof(td3)? 1 : -1];

void f()
{
  _Decimal32 d32[ARRAY_SIZE];
  _Decimal64 d64[ARRAY_SIZE];
  _Decimal128 d128[ARRAY_SIZE];

  int d32_array_size [sizeof(d32) == ARRAY_SIZE * sizeof(_Decimal32) ? 1 : -1];
  int d64_array_size [sizeof(d64) == ARRAY_SIZE * sizeof(_Decimal64) ? 1 : -1];
  int d128_array_size [sizeof(d128) == ARRAY_SIZE * sizeof(_Decimal128)? 1 : -1];
}
