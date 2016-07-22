/* Check that thread pointer relative memory accesses are converted to
   gbr displacement address modes.  If we see a gbr register store
   instruction something is not working properly.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-times "stc\tgbr" 0 } } */

/* ---------------------------------------------------------------------------
  Simple GBR load.
*/
#define func(name, rettype, type, disp)\
  rettype \
  name ## _tp_load (void) \
  { \
    type* tp = (type*)__builtin_thread_pointer (); \
    return tp[disp]; \
  }

func (test00, int, int, 0)
func (test01, int, int, 5)
func (test02, int, int, 255)

func (test03, int, short, 0)
func (test04, int, short, 5)
func (test05, int, short, 255)

func (test06, int, char, 0)
func (test07, int, char, 5)
func (test08, int, char, 255)

func (test09, int, unsigned int, 0)
func (test10, int, unsigned int, 5)
func (test11, int, unsigned int, 255)

func (test12, int, unsigned short, 0)
func (test13, int, unsigned short, 5)
func (test14, int, unsigned short, 255)

func (test15, int, unsigned char, 0)
func (test16, int, unsigned char, 5)
func (test17, int, unsigned char, 255)

func (test18, long long, long long, 0)
func (test19, long long, long long, 5)
func (test20, long long, long long, 127)

func (test21, long long, unsigned long long, 0)
func (test22, long long, unsigned long long, 5)
func (test23, long long, unsigned long long, 127)

#undef func

/* ---------------------------------------------------------------------------
  Simple GBR store.
*/
#define func(name, argtype, type, disp)\
  void \
  name ## _tp_store (argtype a) \
  { \
    type* tp = (type*)__builtin_thread_pointer (); \
    tp[disp] = (type)a; \
  }

func (test00, int, int, 0)
func (test01, int, int, 5)
func (test02, int, int, 255)

func (test03, int, short, 0)
func (test04, int, short, 5)
func (test05, int, short, 255)

func (test06, int, char, 0)
func (test07, int, char, 5)
func (test08, int, char, 255)

func (test09, int, unsigned int, 0)
func (test10, int, unsigned int, 5)
func (test11, int, unsigned int, 255)

func (test12, int, unsigned short, 0)
func (test13, int, unsigned short, 5)
func (test14, int, unsigned short, 255)

func (test15, int, unsigned char, 0)
func (test16, int, unsigned char, 5)
func (test17, int, unsigned char, 255)

func (test18, long long, long long, 0)
func (test19, long long, long long, 5)
func (test20, long long, long long, 127)

func (test21, long long, unsigned long long, 0)
func (test22, long long, unsigned long long, 5)
func (test23, long long, unsigned long long, 127)

#undef func

/* ---------------------------------------------------------------------------
  Arithmetic on the result of a GBR load.
*/
#define func(name, retargtype, type, disp, op, opname)\
  retargtype \
  name ## _tp_load_arith_ ##opname (retargtype a) \
  { \
    type* tp = (type*)__builtin_thread_pointer (); \
    return tp[disp] op a; \
  }

#define funcs(op, opname) \
  func (test00, int, int, 0, op, opname) \
  func (test01, int, int, 5, op, opname) \
  func (test02, int, int, 255, op, opname) \
  func (test03, int, short, 0, op, opname) \
  func (test04, int, short, 5, op, opname) \
  func (test05, int, short, 255, op, opname) \
  func (test06, int, char, 0, op, opname) \
  func (test07, int, char, 5, op, opname) \
  func (test08, int, char, 255, op, opname) \
  func (test09, int, unsigned int, 0, op, opname) \
  func (test10, int, unsigned int, 5, op, opname) \
  func (test11, int, unsigned int, 255, op, opname) \
  func (test12, int, unsigned short, 0, op, opname) \
  func (test13, int, unsigned short, 5, op, opname) \
  func (test14, int, unsigned short, 255, op, opname) \
  func (test15, int, unsigned char, 0, op, opname) \
  func (test16, int, unsigned char, 5, op, opname) \
  func (test17, int, unsigned char, 255, op, opname) \
  func (test18, long long, long long, 0, op, opname) \
  func (test19, long long, long long, 5, op, opname) \
  func (test20, long long, long long, 127, op, opname) \
  func (test21, long long, unsigned long long, 0, op, opname) \
  func (test22, long long, unsigned long long, 5, op, opname) \
  func (test23, long long, unsigned long long, 127, op, opname) \

funcs (+, plus)
funcs (-, minus)
funcs (*, mul)
funcs (&, and)
funcs (|, or)
funcs (^, xor)

#undef funcs
#undef func

/* ---------------------------------------------------------------------------
  Arithmetic of the result of two GBR loads.
*/
#define func(name, rettype, type, disp0, disp1, op, opname)\
  rettype \
  name ## _tp_load_load_arith_ ##opname (void) \
  { \
    type* tp = (type*)__builtin_thread_pointer (); \
    return tp[disp0] op tp[disp1]; \
  }

#define funcs(op, opname) \
  func (test00, int, int, 0, 5, op, opname) \
  func (test02, int, int, 1, 255, op, opname) \
  func (test03, int, short, 0, 5, op, opname) \
  func (test05, int, short, 1, 255, op, opname) \
  func (test06, int, char, 0, 5, op, opname) \
  func (test08, int, char, 1, 255, op, opname) \
  func (test09, int, unsigned int, 0, 5, op, opname) \
  func (test11, int, unsigned int, 1, 255, op, opname) \
  func (test12, int, unsigned short, 0, 5, op, opname) \
  func (test14, int, unsigned short, 1, 255, op, opname) \
  func (test15, int, unsigned char, 0, 5, op, opname) \
  func (test17, int, unsigned char, 1, 255, op, opname) \
  func (test18, long long, long long, 0, 5, op, opname) \
  func (test19, long long, long long, 1, 127, op, opname) \
  func (test20, long long, unsigned long long, 0, 5, op, opname) \
  func (test21, long long, unsigned long long, 1, 127, op, opname) \

funcs (+, plus)
funcs (-, minus)
funcs (*, mul)
funcs (&, and)
funcs (|, or)
funcs (^, xor)

#undef funcs
#undef func

/* ---------------------------------------------------------------------------
  GBR load GBR store copy.
*/

#define func(name, type, disp0, disp1)\
  void \
  name ## _tp_copy (void) \
  { \
    type* tp = (type*)__builtin_thread_pointer (); \
    tp[disp0] = tp[disp1]; \
  }

func (test00, int, 0, 5)
func (test02, int, 1, 255)
func (test03, short, 0, 5)
func (test05, short, 1, 255)
func (test06, char, 0, 5)
func (test08, char, 1, 255)
func (test09, unsigned int, 0, 5)
func (test11, unsigned int, 1, 255)
func (test12, unsigned short, 0, 5)
func (test14, unsigned short, 1, 255)
func (test15, unsigned char, 0, 5)
func (test17, unsigned char, 1, 255)
func (test18, long long, 0, 5)
func (test19, long long, 1, 127)
func (test20, unsigned long long, 0, 5)
func (test21, unsigned long long, 1, 127)

#undef func

/* ---------------------------------------------------------------------------
  GBR load, arithmetic, GBR store
*/

#define func(name, argtype, type, disp, op, opname)\
  void \
  name ## _tp_load_arith_store_ ##opname (argtype a) \
  { \
    type* tp = (type*)__builtin_thread_pointer (); \
    tp[disp] op a; \
  }

#define funcs(op, opname) \
  func (test00, int, int, 0, op, opname) \
  func (test01, int, int, 5, op, opname) \
  func (test02, int, int, 255, op, opname) \
  func (test03, int, short, 0, op, opname) \
  func (test04, int, short, 5, op, opname) \
  func (test05, int, short, 255, op, opname) \
  func (test06, int, char, 0, op, opname) \
  func (test07, int, char, 5, op, opname) \
  func (test08, int, char, 255, op, opname) \
  func (test09, int, unsigned int, 0, op, opname) \
  func (test10, int, unsigned int, 5, op, opname) \
  func (test11, int, unsigned int, 255, op, opname) \
  func (test12, int, unsigned short, 0, op, opname) \
  func (test13, int, unsigned short, 5, op, opname) \
  func (test14, int, unsigned short, 255, op, opname) \
  func (test15, int, unsigned char, 0, op, opname) \
  func (test16, int, unsigned char, 5, op, opname) \
  func (test17, int, unsigned char, 255, op, opname) \
  func (test18, long long, long long, 0, op, opname) \
  func (test19, long long, long long, 5, op, opname) \
  func (test20, long long, long long, 127, op, opname) \
  func (test21, long long, unsigned long long, 0, op, opname) \
  func (test22, long long, unsigned long long, 5, op, opname) \
  func (test23, long long, unsigned long long, 127, op, opname) \

funcs (+=, plus)
funcs (-=, minus)
funcs (*=, mul)
funcs (&=, and)
funcs (|=, or)
funcs (^=, xor)
