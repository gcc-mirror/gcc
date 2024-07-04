/* { dg-do compile } */
/* { dg-options "-march=rv32i_zfbfmin -mabi=ilp32f -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i_zfbfmin -mabi=lp64f -O" { target { rv64 } } } */


/* 1) float -> BF16
 *    hf    -> bf == hf -> sf -> bf      fcvt.h.s + fcvt.bf16.s
 *    sf    -> bf ==       sf -> bf                 fcvt.bf16.s
 *    df    -> bf == df -> sf -> bf  __truncdfsf2 + fcvt.bf16.s
 *    tf    -> bf == tf -> sf -> bf  __trunctfsf2 + fcvt.bf16.s
*/

/* 2) BF16  -> float
 *    bf    -> hf == bf -> sf -> hf  fcvt.s.bf16 + fcvt.s.h
 *    bf    -> sf == bf -> sf        fcvt.s.bf16
 *    bf    -> df == bf -> sf -> df  fcvt.s.bf16 + __extendsfdf2
 *    bf    -> tf == bf -> sf -> tf  fcvt.s.bf16 + __extendsftf2
*/

extern   __bf16    bf;
extern   _Float16  hf;
extern      float  sf;
extern      double df;
extern long double tf;

void hf_to_bf () { bf = hf; }
void bf_to_hf () { hf = bf; }

void sf_to_bf () { bf = sf; }
void bf_to_sf () { sf = bf; }

void df_to_bf () { bf = df; } 
void bf_to_df () { df = bf; }

void tf_to_bf () { bf = tf; }
void bf_to_tf () { tf = bf; }

/* { dg-final { scan-assembler-times "fcvt.bf16.s" 4 } } */
/* { dg-final { scan-assembler-times "fcvt.s.bf16" 4 } } */
/* { dg-final { scan-assembler-times "fcvt.h.s" 1 } } */
/* { dg-final { scan-assembler-times "fcvt.s.h" 1 } } */
/* { dg-final { scan-assembler-times "call\t__truncdfsf2" 1 } } */
/* { dg-final { scan-assembler-times "call\t__trunctfsf2" 1 } } */
/* { dg-final { scan-assembler-times "call\t__extendsfdf2" 1 } } */
/* { dg-final { scan-assembler-times "call\t__extendsftf2" 1 } } */
