/* { dg-do compile } */
/* { dg-options "-O2 -fno-rtti -fno-exceptions -fno-strict-aliasing -fdump-tree-vrp2" } */


extern  void JS_Assert();
typedef enum {
eax,         ecx,         edx,         ebx,         esp,         ebp,
esi,         edi     }
RegisterID;
union StateRemat {
  RegisterID reg_;
  int offset_;
};
static StateRemat FromRegister(RegisterID reg) {
  StateRemat sr;
  sr.reg_ = reg;
  return sr;
}
static StateRemat FromAddress3(int address) {
  StateRemat sr;
    sr.offset_ = address;
  //sr.offset_ = 0;
  if (address < 46 &&    address >= 0) {
    JS_Assert();
  }
  return sr;
}
struct FrameState {
  StateRemat dataRematInfo2(bool y, int z) {
    if (y)         return FromRegister(RegisterID(1));
    return FromAddress3(z);
  }
};
FrameState frame;
StateRemat x;
void jsop_setelem(bool y, int z) {
  x = frame.dataRematInfo2(y, z);
}

/* { dg-final { scan-tree-dump-times "Folding predicate.*45" 0 "vrp2"} } */
/* { dg-final { cleanup-tree-dump "vrp2" } } */
