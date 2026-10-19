/* { dg-do compile } */
/* { dg-options "-O2" } */

enum E {};
bool getKnownMinValue();
struct TypeSize {
  bool Scalable;
} getSizeInBits_SizeTable;
extern TypeSize SizeTable[];
void isKnownGT(TypeSize);
struct MVT {
  E SimpleTy;
} V, LowerFCOPYSIGN___trans_tmp_10;
E getSizeInBits_e_0;
static void knownBitsGT(MVT e, MVT VT) {
  TypeSize __trans_tmp_4, __trans_tmp_1;
  switch (VT.SimpleTy)
  case 0:
  case 2:
    __trans_tmp_1 = getSizeInBits_SizeTable;
  switch (e.SimpleTy) {
  case 0:
  case 2:
  case 3:
    __builtin_unreachable();
  default:
    __trans_tmp_4 = SizeTable[e.SimpleTy];
  }
  isKnownGT(__trans_tmp_4);
  isKnownGT(__trans_tmp_1);
}
static bool knownBitsLT(MVT e) {
  switch (getSizeInBits_e_0)
  case 2:
    __builtin_unreachable();
  switch (e.SimpleTy)
  case 0:
  case 2:
  case 3:
    __builtin_unreachable();
  TypeSize LHS;
  if (LHS.Scalable)
    return getKnownMinValue();
  return false;
}
bool LowerFCOPYSIGN_bl;
void LowerFCOPYSIGN() {
  MVT __trans_tmp_12, __trans_tmp_11 = LowerFCOPYSIGN___trans_tmp_10,
                      __trans_tmp_7 = __trans_tmp_11, VT;
  LowerFCOPYSIGN_bl = knownBitsLT(__trans_tmp_7);
  if (LowerFCOPYSIGN_bl)
    LowerFCOPYSIGN___trans_tmp_10 = V;
  __trans_tmp_12 = LowerFCOPYSIGN___trans_tmp_10;
  MVT __trans_tmp_8 = __trans_tmp_12;
  knownBitsGT(__trans_tmp_8, VT);
}
