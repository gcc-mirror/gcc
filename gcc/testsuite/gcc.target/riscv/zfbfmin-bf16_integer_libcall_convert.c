/* { dg-do compile } */
/* { dg-options "-march=rv32i_zfbfmin -mabi=ilp32f -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i_zfbfmin -mabi=lp64f -O" { target { rv64 } } } */

/* 1) Integer     -> BF16 
 *    qi/hi       -> bf           fcvt.s.w  + fcvt.bf16.s
 *    uqi/uhi     -> bf           fcvt.s.wu + fcvt.bf16.s
 *
 *    si/di/ti    -> bf           __float[s|d|t]ibf
 *    usi/udi/uti -> bf           __floatun[s|d|t]ibf
*/

/* 2) BF16        -> Integer
 *    bf          -> qi/hi/si/di     fcvt.s.bf16 + fcvt.[w|l].s
 *    bf          -> uqi/uhi/usi/udi fcvt.s.bf16 + fcvt.[w|l]u.s
 *    bf          -> ti/uti          fcvt.s.bf16 + __fix[uns]sfti
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

/* { dg-final { scan-assembler-times "fcvt.bf16.s" 4 } } */
/* { dg-final { scan-assembler-times "fcvt.s.bf16" 8 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "fcvt.s.bf16" 10 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__floatsibf" 1  } } */
/* { dg-final { scan-assembler-times "call\t__floatunsibf" 1  } } */
/* { dg-final { scan-assembler-times "call\t__floatdibf" 1  } } */
/* { dg-final { scan-assembler-times "call\t__floatundibf" 1  } } */
