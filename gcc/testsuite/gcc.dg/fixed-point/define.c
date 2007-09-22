/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1169 7.18a.3 - Precision macros.

   Check if defined fixed-point constants are ok.  */

#include <stdfix.h>

short fract sf = SFRACT_MAX;
fract f = FRACT_MAX;
long fract lf = LFRACT_MAX;
long long fract llf = LLFRACT_MAX;

unsigned short fract usf = USFRACT_MAX;
unsigned fract uf = UFRACT_MAX;
unsigned long fract ulf = ULFRACT_MAX;
unsigned long long fract ullf = ULLFRACT_MAX;

sat short fract Ssf = SFRACT_MAX;
sat fract Sf = FRACT_MAX;
sat long fract Slf = LFRACT_MAX;
sat long long fract Sllf = LLFRACT_MAX;

sat unsigned short fract Susf = USFRACT_MAX;
sat unsigned fract Suf = UFRACT_MAX;
sat unsigned long fract Sulf = ULFRACT_MAX;
sat unsigned long long fract Sullf = ULLFRACT_MAX;

short fract sfm = SFRACT_MIN;
fract fm = FRACT_MIN;
long fract lfm = LFRACT_MIN;
long long fract llfm = LLFRACT_MIN;

unsigned short fract usfm = USFRACT_MIN;
unsigned fract ufm = UFRACT_MIN;
unsigned long fract ulfm = ULFRACT_MIN;
unsigned long long fract ullfm = ULLFRACT_MIN;

sat short fract Ssfm = SFRACT_MIN;
sat fract Sfm = FRACT_MIN;
sat long fract Slfm = LFRACT_MIN;
sat long long fract Sllfm = LLFRACT_MIN;

sat unsigned short fract Susfm = USFRACT_MIN;
sat unsigned fract Sufm = UFRACT_MIN;
sat unsigned long fract Sulfm = ULFRACT_MIN;
sat unsigned long long fract Sullfm = ULLFRACT_MIN;

short fract sfE = SFRACT_EPSILON;
fract fE = FRACT_EPSILON;
long fract lfE = LFRACT_EPSILON;
long long fract llfE = LLFRACT_EPSILON;

unsigned short fract usfE = USFRACT_EPSILON;
unsigned fract ufE = UFRACT_EPSILON;
unsigned long fract ulfE = ULFRACT_EPSILON;
unsigned long long fract ullfE = ULLFRACT_EPSILON;

sat short fract SsfE = SFRACT_EPSILON;
sat fract SfE = FRACT_EPSILON;
sat long fract SlfE = LFRACT_EPSILON;
sat long long fract SllfE = LLFRACT_EPSILON;

sat unsigned short fract SusfE = USFRACT_EPSILON;
sat unsigned fract SufE = UFRACT_EPSILON;
sat unsigned long fract SulfE = ULFRACT_EPSILON;
sat unsigned long long fract SullfE = ULLFRACT_EPSILON;

short accum sk = SACCUM_MAX;
accum k = ACCUM_MAX;
long accum lk = LACCUM_MAX;
long long accum llk = LLACCUM_MAX;

unsigned short accum usk = USACCUM_MAX;
unsigned accum uk = UACCUM_MAX;
unsigned long accum ulk = ULACCUM_MAX;
unsigned long long accum ullk = ULLACCUM_MAX;

sat short accum Ssk = SACCUM_MAX;
sat accum Sk = ACCUM_MAX;
sat long accum Slk = LACCUM_MAX;
sat long long accum Sllk = LLACCUM_MAX;

sat unsigned short accum Susk = USACCUM_MAX;
sat unsigned accum Suk = UACCUM_MAX;
sat unsigned long accum Sulk = ULACCUM_MAX;
sat unsigned long long accum Sullk = ULLACCUM_MAX;

short accum skm = SACCUM_MIN;
accum km = ACCUM_MIN;
long accum lkm = LACCUM_MIN;
long long accum llkm = LLACCUM_MIN;

unsigned short accum uskm = USACCUM_MIN;
unsigned accum ukm = UACCUM_MIN;
unsigned long accum ulkm = ULACCUM_MIN;
unsigned long long accum ullkm = ULLACCUM_MIN;

sat short accum Sskm = SACCUM_MIN;
sat accum Skm = ACCUM_MIN;
sat long accum Slkm = LACCUM_MIN;
sat long long accum Sllkm = LLACCUM_MIN;

sat unsigned short accum Suskm = USACCUM_MIN;
sat unsigned accum Sukm = UACCUM_MIN;
sat unsigned long accum Sulkm = ULACCUM_MIN;
sat unsigned long long accum Sullkm = ULLACCUM_MIN;

short accum skE = SACCUM_EPSILON;
accum kE = ACCUM_EPSILON;
long accum lkE = LACCUM_EPSILON;
long long accum llkE = LLACCUM_EPSILON;

unsigned short accum uskE = USACCUM_EPSILON;
unsigned accum ukE = UACCUM_EPSILON;
unsigned long accum ulkE = ULACCUM_EPSILON;
unsigned long long accum ullkE = ULLACCUM_EPSILON;

sat short accum SskE = SACCUM_EPSILON;
sat accum SkE = ACCUM_EPSILON;
sat long accum SlkE = LACCUM_EPSILON;
sat long long accum SllkE = LLACCUM_EPSILON;

sat unsigned short accum SuskE = USACCUM_EPSILON;
sat unsigned accum SukE = UACCUM_EPSILON;
sat unsigned long accum SulkE = ULACCUM_EPSILON;
sat unsigned long long accum SullkE = ULLACCUM_EPSILON;

int fbit_sf = SFRACT_FBIT;
int fbit_f = FRACT_FBIT;
int fbit_lf = LFRACT_FBIT;
int fbit_llf = LLFRACT_FBIT;

int fbit_usf = USFRACT_FBIT;
int fbit_uf = UFRACT_FBIT;
int fbit_ulf = ULFRACT_FBIT;
int fbit_ullf = ULLFRACT_FBIT;

int fbit_sk = SACCUM_FBIT;
int fbit_k = ACCUM_FBIT;
int fbit_lk = LACCUM_FBIT;
int fbit_llk = LLACCUM_FBIT;

int fbit_usk = USACCUM_FBIT;
int fbit_uk = UACCUM_FBIT;
int fbit_ulk = ULACCUM_FBIT;
int fbit_ullk = ULLACCUM_FBIT;

int ibit_sk = SACCUM_IBIT;
int ibit_k = ACCUM_IBIT;
int ibit_lk = LACCUM_IBIT;
int ibit_llk = LLACCUM_IBIT;

int ibit_usk = USACCUM_IBIT;
int ibit_uk = UACCUM_IBIT;
int ibit_ulk = ULACCUM_IBIT;
int ibit_ullk = ULLACCUM_IBIT;
