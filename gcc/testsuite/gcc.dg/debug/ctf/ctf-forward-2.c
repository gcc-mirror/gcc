/* CTF forward type is generated for forward declarations of enum types in C.
   
   Check that the ctf-kind of CTF_K_FOWARD type is CTF_K_ENUM.
   For forward types, the compiler encodes the CTF kind in the ctt_type field.
   CTF_K_FORWARD is used as the CTF type as usual in the ctt_info.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x26000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x8\[\t \]+\[^\n\]*ctt_size or ctt_type" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"vibgyor.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

enum vibgyor;

char * (*get_color_name) (enum vibgyor);
