/* { dg-do compile } */
/* { dg-options "-march=rv32i -mabi=ilp32 -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i -mabi=lp64 -O" { target { rv64 } } } */


/* 1) float       -> BF16
 *    hf/sf/df/tf -> bf                   (call   __trunc[h|s|d|t]fbf2)
*/

/* 2) BF16        -> float
 *    bf          -> hf == sf -> hf       (call   __truncsfhf2)
 *    bf          -> sf                   (call   __extendbfsf2)
 *    bf          -> df == bf -> sf -> df (call   __extendbfsf2 && __extendsfdf2)
 *    bf          -> tf == bf -> sf -> tf (call   __extendbfsf2 && __extendsftf2)
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

/* { dg-final { scan-assembler-times "call\t__extendbfsf2" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__extendbfsf2" 3 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__extendsfdf2" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__extendsfdf2" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__extendsftf2" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__extendsftf2" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__truncsfhf2" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__truncsfhf2" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__trunchfbf2" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__trunchfbf2" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__truncsfbf2" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__truncsfbf2" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__truncdfbf2" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__truncdfbf2" 1 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "call\t__trunctfbf2" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "call\t__trunctfbf2" 1 { target { rv64 } } } } */
