/* Check that addressing modes for atomics are generated as expected.  */

#define concat_1(x, y) x ## y
#define concat(x, y) concat_1 (x, y)
#define makefuncname(name) concat (concat (test_, __LINE__), name)

#define emitfuncs(name,val,off)\
  char makefuncname (_0) (char* mem)\
  {\
    return name (mem + off, val, __ATOMIC_ACQ_REL);\
  }\
  char makefuncname (_1) (void)\
  {\
    char* mem = (char*)__builtin_thread_pointer ();\
    return name (mem + off, val, __ATOMIC_ACQ_REL);\
  }\
  short makefuncname (_2) (short* mem)\
  {\
    return name (mem + off, val, __ATOMIC_ACQ_REL);\
  }\
  short makefuncname (_3) (void)\
  {\
    short* mem = (short*)__builtin_thread_pointer ();\
    return name (mem + off, val, __ATOMIC_ACQ_REL);\
  }\
  int makefuncname (_4) (int* mem)\
  {\
    return name (mem + off, val, __ATOMIC_ACQ_REL);\
  }\
  int makefuncname (_5) (void)\
  {\
    int* mem = (int*)__builtin_thread_pointer ();\
    return name (mem + off, val, __ATOMIC_ACQ_REL);\
  }\

emitfuncs (__atomic_add_fetch, 1, 0)
emitfuncs (__atomic_add_fetch, 1, 4)

emitfuncs (__atomic_fetch_add, 1, 0)
emitfuncs (__atomic_fetch_add, 1, 4)

emitfuncs (__atomic_sub_fetch, 1, 0)
emitfuncs (__atomic_sub_fetch, 1, 4)
emitfuncs (__atomic_fetch_sub, 1, 0)
emitfuncs (__atomic_fetch_sub, 1, 4)

emitfuncs (__atomic_and_fetch, 1, 0)
emitfuncs (__atomic_and_fetch, 1, 4)
emitfuncs (__atomic_fetch_and, 1, 0)
emitfuncs (__atomic_fetch_and, 1, 4)

emitfuncs (__atomic_or_fetch, 1, 0)
emitfuncs (__atomic_or_fetch, 1, 4)
emitfuncs (__atomic_fetch_or, 1, 0)
emitfuncs (__atomic_fetch_or, 1, 4)

emitfuncs (__atomic_xor_fetch, 1, 0)
emitfuncs (__atomic_xor_fetch, 1, 4)
emitfuncs (__atomic_fetch_xor, 1, 0)
emitfuncs (__atomic_fetch_xor, 1, 4)

emitfuncs (__atomic_nand_fetch, 1, 0)
emitfuncs (__atomic_nand_fetch, 1, 4)
emitfuncs (__atomic_fetch_nand, 1, 0)
emitfuncs (__atomic_fetch_nand, 1, 4)

emitfuncs (__atomic_xor_fetch, -1, 0)
emitfuncs (__atomic_xor_fetch, -1, 4)
emitfuncs (__atomic_fetch_xor, -1, 0)
emitfuncs (__atomic_fetch_xor, -1, 4)

emitfuncs (__atomic_nand_fetch, -1, 0)
emitfuncs (__atomic_nand_fetch, -1, 4)
emitfuncs (__atomic_fetch_nand, -1, 0)
emitfuncs (__atomic_fetch_nand, -1, 4)

#undef emitfuncs
#define emitfuncs(off)\
  char makefuncname (_6) (char* mem)\
  {\
    char expected = 1;\
    char desired = 5;\
    return __atomic_compare_exchange (mem + off, &expected, &desired, 0,\
				      __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);\
  }\
  char makefuncname (_7) (void)\
  {\
    char* mem = (char*)__builtin_thread_pointer ();\
    char expected = 1;\
    char desired = 5;\
    return __atomic_compare_exchange (mem + off, &expected, &desired, 0,\
				      __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);\
  }\
  short makefuncname (_8) (short* mem)\
  {\
    short expected = 1;\
    short desired = 5;\
    return __atomic_compare_exchange (mem + off, &expected, &desired, 0,\
				      __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);\
  }\
  short makefuncname (_9) (void)\
  {\
    short* mem = (short*)__builtin_thread_pointer ();\
    short expected = 1;\
    short desired = 5;\
    return __atomic_compare_exchange (mem + off, &expected, &desired, 0,\
				      __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);\
  }\
  int makefuncname (_10) (int* mem)\
  {\
    int expected = 1;\
    int desired = 5;\
    return __atomic_compare_exchange (mem + off, &expected, &desired, 0,\
				      __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);\
  }\
  int makefuncname (_11) (void)\
  {\
    int* mem = (int*)__builtin_thread_pointer ();\
    int expected = 1;\
    int desired = 5;\
    return __atomic_compare_exchange (mem + off, &expected, &desired, 0,\
				      __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);\
  }\
  char makefuncname (_12) (char* mem)\
  {\
    char newval = 5;\
    char prevval;\
    __atomic_exchange (mem + off, &newval, &prevval, __ATOMIC_ACQ_REL);\
    return prevval;\
  }\
  char makefuncname (_13) (void)\
  {\
    char* mem = (char*)__builtin_thread_pointer ();\
    char newval = 5;\
    char prevval;\
    __atomic_exchange (mem + off, &newval, &prevval, __ATOMIC_ACQ_REL);\
    return prevval;\
  }\
  short makefuncname (_14) (short* mem)\
  {\
    short newval = 5;\
    short prevval;\
    __atomic_exchange (mem + off, &newval, &prevval, __ATOMIC_ACQ_REL);\
    return prevval;\
  }\
  short makefuncname (_15) (void)\
  {\
    short* mem = (short*)__builtin_thread_pointer ();\
    short newval = 5;\
    short prevval;\
    __atomic_exchange (mem + off, &newval, &prevval, __ATOMIC_ACQ_REL);\
    return prevval;\
  }\
  int makefuncname (_16) (int* mem)\
  {\
    int newval = 5;\
    int prevval;\
    __atomic_exchange (mem + off, &newval, &prevval, __ATOMIC_ACQ_REL);\
    return prevval;\
  }\
  int makefuncname (_17) (void)\
  {\
    int* mem = (int*)__builtin_thread_pointer ();\
    int newval = 5;\
    int prevval;\
    __atomic_exchange (mem + off, &newval, &prevval, __ATOMIC_ACQ_REL);\
    return prevval;\
  }\

emitfuncs (0)
emitfuncs (4)
