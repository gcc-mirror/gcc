/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1169 6.3.1.3a - Fixed-point types (NEW CLAUSE).

   Check if all types are ok.  */

short _Fract q0;
_Fract q1;
long _Fract q2;
long long _Fract q3;
unsigned short _Fract q4;
unsigned _Fract q5;
unsigned long _Fract q6;
unsigned long long _Fract q7;
_Sat short _Fract sq0;
_Sat _Fract sq1;
_Sat long _Fract sq2;
_Sat long long _Fract sq3;
_Sat unsigned short _Fract sq4;
_Sat unsigned _Fract sq5;
_Sat unsigned long _Fract sq6;
_Sat unsigned long long _Fract sq7;

short _Accum a0;
_Accum a1;
long _Accum a2;
long long _Accum a3;
unsigned short _Accum a4;
unsigned _Accum a5;
unsigned long _Accum a6;
unsigned long long _Accum a7;
_Sat short _Accum sa0;
_Sat _Accum sa1;
_Sat long _Accum sa2;
_Sat long long _Accum sa3;
_Sat unsigned short _Accum sa4;
_Sat unsigned _Accum sa5;
_Sat unsigned long _Accum sa6;
_Sat unsigned long long _Accum sa7;
