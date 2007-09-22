/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* Check if GCC generates warnings when overflows for mul and div.  */

short _Fract sf0 = __SFRACT_MIN__ * __SFRACT_MIN__; /* { dg-warning "overflow" } */
_Fract f0 = __FRACT_MIN__ * __FRACT_MIN__; /* { dg-warning "overflow" } */
long _Fract lf0 = __LFRACT_MIN__ * __LFRACT_MIN__; /* { dg-warning "overflow" } */
long long _Fract llf0 = __LLFRACT_MIN__ * __LLFRACT_MIN__; /* { dg-warning "overflow" } */
short _Fract sf1 = __SFRACT_MAX__ * __SFRACT_MIN__;
_Fract f1 = __FRACT_MAX__ * __FRACT_MIN__;
long _Fract lf1 = __LFRACT_MAX__ * __LFRACT_MIN__;
long long _Fract llf1 = __LLFRACT_MAX__ * __LLFRACT_MIN__;
short _Fract sf2 = __SFRACT_MIN__ / __SFRACT_MIN__; /* { dg-warning "overflow" } */
_Fract f2 = __FRACT_MIN__ / __FRACT_MIN__; /* { dg-warning "overflow" } */
long _Fract lf2 = __LFRACT_MIN__ / __LFRACT_MIN__; /* { dg-warning "overflow" } */
long long _Fract llf2 = __LLFRACT_MIN__ / __LLFRACT_MIN__; /* { dg-warning "overflow" } */
short _Fract sf3 = __SFRACT_MAX__ / __SFRACT_MIN__;
_Fract f3 = __FRACT_MAX__ / __FRACT_MIN__;
long _Fract lf3 = __LFRACT_MAX__ / __LFRACT_MIN__;
long long _Fract llf3 = __LLFRACT_MAX__ / __LLFRACT_MIN__;

unsigned short _Fract usf0 = __USFRACT_MIN__ * __USFRACT_MIN__;
unsigned _Fract uf0 = __UFRACT_MIN__ * __UFRACT_MIN__;
unsigned long _Fract ulf0 = __ULFRACT_MIN__ * __ULFRACT_MIN__;
unsigned long long _Fract ullf0 = __ULLFRACT_MIN__ * __ULLFRACT_MIN__;
unsigned short _Fract usf1 = __USFRACT_MAX__ * __USFRACT_MIN__;
unsigned _Fract uf1 = __UFRACT_MAX__ * __UFRACT_MIN__;
unsigned long _Fract ulf1 = __ULFRACT_MAX__ * __ULFRACT_MIN__;
unsigned long long _Fract ullf1 = __ULLFRACT_MAX__ * __ULLFRACT_MIN__;
unsigned short _Fract usf2 = __USFRACT_MAX__ / __USFRACT_MAX__; /* { dg-warning "overflow" } */
unsigned _Fract uf2 = __UFRACT_MAX__ / __UFRACT_MAX__; /* { dg-warning "overflow" } */
unsigned long _Fract ulf2 = __ULFRACT_MAX__ / __ULFRACT_MAX__; /* { dg-warning "overflow" } */
unsigned long long _Fract ullf2 = __ULLFRACT_MAX__ / __ULLFRACT_MAX__; /* { dg-warning "overflow" } */
unsigned short _Fract usf3 = __USFRACT_MIN__ / __USFRACT_MAX__;
unsigned _Fract uf3 = __UFRACT_MIN__ / __UFRACT_MAX__;
unsigned long _Fract ulf3 = __ULFRACT_MIN__ / __ULFRACT_MAX__;
unsigned long long _Fract ullf3 = __ULLFRACT_MIN__ / __ULLFRACT_MAX__;

short _Accum sa0 = __SACCUM_MIN__ * __SACCUM_MIN__; /* { dg-warning "overflow" } */
_Accum a0 = __ACCUM_MIN__ * __ACCUM_MIN__; /* { dg-warning "overflow" } */
long _Accum la0 = __LACCUM_MIN__ * __LACCUM_MIN__; /* { dg-warning "overflow" } */
long long _Accum lla0 = __LLACCUM_MIN__ * __LLACCUM_MIN__; /* { dg-warning "overflow" } */
short _Accum sa1 = __SACCUM_MAX__ * 0.1hk;
_Accum a1 = __ACCUM_MAX__ * 0.1k;
long _Accum la1 = __LACCUM_MAX__ * 0.1lk;
long long _Accum lla1 = __LLACCUM_MAX__ * 0.1llk;
short _Accum sa2 = __SACCUM_MIN__ / 0.1hk; /* { dg-warning "overflow" } */
_Accum a2 = __ACCUM_MIN__ / 0.1k; /* { dg-warning "overflow" } */
long _Accum la2 = __LACCUM_MIN__ / 0.1lk; /* { dg-warning "overflow" } */
long long _Accum lla2 = __LLACCUM_MIN__ / 0.1llk; /* { dg-warning "overflow" } */
short _Accum sa3 = __SACCUM_MAX__ / __SACCUM_MIN__;
_Accum a3 = __ACCUM_MAX__ / __ACCUM_MIN__;
long _Accum la3 = __LACCUM_MAX__ / __LACCUM_MIN__;
long long _Accum lla3 = __LLACCUM_MAX__ / __LLACCUM_MIN__;

unsigned short _Accum usa0 = __USACCUM_MIN__ * __USACCUM_MIN__;
unsigned _Accum ua0 = __UACCUM_MIN__ * __UACCUM_MIN__;
unsigned long _Accum ula0 = __ULACCUM_MIN__ * __ULACCUM_MIN__;
unsigned long long _Accum ulla0 = __ULLACCUM_MIN__ * __ULLACCUM_MIN__;
unsigned short _Accum usa1 = __USACCUM_MAX__ * __USACCUM_MAX__; /* { dg-warning "overflow" } */
unsigned _Accum ua1 = __UACCUM_MAX__ * __UACCUM_MAX__; /* { dg-warning "overflow" } */
unsigned long _Accum ula1 = __ULACCUM_MAX__ * __ULACCUM_MAX__; /* { dg-warning "overflow" } */
unsigned long long _Accum ulla1 = __ULLACCUM_MAX__ * __ULLACCUM_MAX__; /* { dg-warning "overflow" } */
unsigned short _Accum usa2 = __USACCUM_MAX__ / 0.5hk; /* { dg-warning "overflow" } */
unsigned _Accum ua2 = __UACCUM_MAX__ / 0.5k; /* { dg-warning "overflow" } */
unsigned long _Accum ula2 = __ULACCUM_MAX__ / 0.5lk; /* { dg-warning "overflow" } */
unsigned long long _Accum ulla2 = __ULLACCUM_MAX__ / 0.5llk; /* { dg-warning "overflow" } */
unsigned short _Accum usa3 = __USACCUM_MIN__ / __USACCUM_MAX__;
unsigned _Accum ua3 = __UACCUM_MIN__ / __UACCUM_MAX__;
unsigned long _Accum ula3 = __ULACCUM_MIN__ / __ULACCUM_MAX__;
unsigned long long _Accum ulla3 = __ULLACCUM_MIN__ / __ULLACCUM_MAX__;
