/* Check that atomic not ops are generated.  */

#define emitfuncs(name)\
  void test_ ## name ## _0 (char* mem)\
  {\
    name (mem, -1, __ATOMIC_ACQ_REL);\
  }\
  void test_ ## name ## _1 (short* mem)\
  {\
    name (mem, -1, __ATOMIC_ACQ_REL);\
  }\
  void test_ ## name ##_2 (int* mem)\
  {\
    name (mem, -1, __ATOMIC_ACQ_REL);\
  }\

emitfuncs (__atomic_xor_fetch)
emitfuncs (__atomic_fetch_xor)

emitfuncs (__atomic_nand_fetch)
emitfuncs (__atomic_fetch_nand)
