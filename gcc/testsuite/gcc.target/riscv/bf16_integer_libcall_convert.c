/* { dg-do compile } */
/* { dg-options "-march=rv32i -mabi=ilp32 -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i -mabi=lp64 -O" { target { rv64 } } } */

/* 1) Integer     -> BF16 
 *    qi/hi/si    -> bf (call	__floatsibf) 
 *    uqi/uhi/usi -> bf (call	__floatunsibf)  
 *    di/ti       -> bf (call	__float[d|t]ibf)
 *    udi/uti     -> bf (call	__floatun[d|t]ibf)
*/

/* 2) BF16        -> Integer
 *    bf          -> qi/hi/si    (call   __fixsfsi)
 *    bf          -> uqi/uhi/usi (call   __fixunssfsi)
 *    bf          -> di/ti       (call   __fixsf[d|t]i)
 *    bf          -> udi/uti     (call   __fixunssf[d|t]i)
*/

extern __bf16 bf;

extern   signed char       qi;
extern unsigned char      uqi;
extern   signed short      hi;
extern unsigned short     uhi;
extern   signed int        si;
extern unsigned int       usi;
extern   signed long long  di;
extern unsigned long long udi;

void qi_to_bf ()  { bf = qi; }
void uqi_to_bf () { bf = uqi; }
void bf_to_qi ()  { qi = bf; }
void bf_to_uqi () { uqi = bf; }

void hi_to_bf ()  { bf = hi; }
void uhi_to_bf () { bf = uhi; }
void bf_to_hi ()  { hi = bf; }
void bf_to_uhi () { uhi = bf; }

void si_to_bf ()  { bf = si; }
void usi_to_bf () { bf = usi; }
void bf_to_si ()  { si = bf; }
void bf_to_usi () { usi = bf; }

void di_to_bf ()  { bf = di; }
void udi_to_bf () { bf = udi; }
void bf_to_di ()  { di = bf; }
void bf_to_udi () { udi = bf; }

#if __riscv_xlen == 64
extern   signed __int128   ti; 
extern unsigned __int128  uti; 
void ti_to_bf ()  { bf = ti; }  /* { dg-final { scan-assembler-times "call\t__floattibf" 1 { target { rv64 } } } } */
void uti_to_bf () { bf = uti; } /* { dg-final { scan-assembler-times "call\t__floatuntibf" 1 { target { rv64 } } } } */
void bf_to_ti ()  { ti = bf; }  /* { dg-final { scan-assembler-times "call\t__fixsfti" 1 { target { rv64 } } } } */
void bf_to_uti () { uti = bf; } /* { dg-final { scan-assembler-times "call\t__fixunssfti" 1 { target { rv64 } } } } */
#endif

/* { dg-final { scan-assembler-times "call\t__fixsfsi" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__fixsfsi" 3 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__fixsfdi" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__fixsfdi" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__fixunssfsi" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__fixunssfsi" 3 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__fixunssfdi" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__fixunssfdi" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__floatsibf" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__floatsibf" 3 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__floatdibf" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__floatdibf" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__floatunsibf" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__floatunsibf" 3 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__floatundibf" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__floatundibf" 1 { target { rv64 } } } } */
