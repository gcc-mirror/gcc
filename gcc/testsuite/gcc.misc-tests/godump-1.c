/* Test -fdump-go-specs option.  */

/* { dg-options "-c -fdump-go-spec=godump-1.out" } */
/* { dg-do compile } */
/* { dg-skip-if "not supported for target" { ! "alpha*-*-* s390*-*-* i?86-*-* x86_64-*-*" } } */
/* { dg-skip-if "not supported for target" { ! lp64 } } */

#include <stdint.h>

/* Necessary quoting in the regexp patters:

     (?n) at beginning of pattern to make ^ and $ work.
     "     ->  \"
     *, +  ->  "*", "+"
     [, ]  ->  "\[", "\]"
     (, )  ->  "\[(\]", "\[)\]"
     {, }  ->  "\{", "\}"
*/

/*** integer based types ***/
typedef char c_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _c_t u?int8$" } } */

char c_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _c_v1 u?int\[0-9\]*$" } } */

c_t c_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _c_v2 _c_t$" } } */

typedef short s_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _s_t int\[0-9\]*$" } } */

short s_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _s_v1 int\[0-9\]*$" } } */

s_t s_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _s_v2 _s_t$" } } */

typedef int i_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _i_t int\[0-9\]*$" } } */

int i_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _i_v1 int\[0-9\]*$" } } */

i_t i_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _i_v2 _i_t$" } } */

typedef long l_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _l_t int\[0-9\]*$" } } */

long l_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _l_v1 int\[0-9\]*$" } } */

l_t l_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _l_v2 _l_t$" } } */

typedef long long ll_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ll_t int\[0-9\]*$" } } */

long long ll_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _ll_v1 int\[0-9\]*$" } } */

ll_t ll_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ll_v2 _ll_t$" } } */

typedef unsigned char uc_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _uc_t uint8$" } } */

unsigned char uc_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _uc_v1 uint\[0-9\]*$" } } */

uc_t uc_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _uc_v2 _uc_t$" } } */

typedef unsigned short us_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _us_t uint\[0-9\]*$" } } */

unsigned short us_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _us_v1 uint\[0-9\]*$" } } */

us_t us_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _us_v2 _us_t$" } } */

typedef unsigned int ui_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ui_t uint\[0-9\]*$" } } */

unsigned int ui_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _ui_v1 uint\[0-9\]*$" } } */

ui_t ui_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ui_v2 _ui_t$" } } */

typedef unsigned long ul_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ul_t uint\[0-9\]*$" } } */

unsigned long ul_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _ul_v1 uint\[0-9\]*$" } } */

ul_t ul_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ul_v2 _ul_t$" } } */

typedef unsigned long long ull_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ull_t uint\[0-9\]*$" } } */

unsigned long long ull_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _ull_v1 uint\[0-9\]*$" } } */

ull_t ull_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ull_v2 _ull_t$" } } */

typedef signed char sc_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _sc_t int8$" } } */

signed char sc_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sc_v1 int\[0-9\]*$" } } */

sc_t sc_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sc_v2 _sc_t$" } } */

typedef signed short ss_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ss_t int\[0-9\]*$" } } */

signed short ss_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _ss_v1 int\[0-9\]*$" } } */

ss_t ss_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ss_v2 _ss_t$" } } */

typedef signed int si_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _si_t int\[0-9\]*$" } } */

signed int si_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _si_v1 int\[0-9\]*$" } } */

si_t si_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _si_v2 _si_t$" } } */

typedef signed long sl_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _sl_t int\[0-9\]*$" } } */

signed long sl_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sl_v1 int\[0-9\]*$" } } */

sl_t sl_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sl_v2 _sl_t$" } } */

typedef signed long long sll_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _sll_t int\[0-9\]*$" } } */

signed long long sll_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sll_v1 int\[0-9\]*$" } } */

sll_t sll_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sll_v2 _sll_t$" } } */

typedef int8_t i8_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _i8_t int8$" } } */

int8_t i8_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _i8_v1 _int8_t$" } } */

i8_t i8_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _i8_v2 _i8_t$" } } */

typedef int16_t i16_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _i16_t int16$" } } */

int16_t i16_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _i16_v1 _int16_t$" } } */

i16_t i16_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _i16_v2 _i16_t$" } } */

typedef int32_t i32_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _i32_t int32$" } } */

int32_t i32_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _i32_v1 _int32_t$" } } */

i32_t i32_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _i32_v2 _i32_t$" } } */

typedef int64_t i64_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _i64_t int64$" } } */

int64_t i64_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _i64_v1 _int64_t$" } } */

i64_t i64_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _i64_v2 _i64_t$" } } */

typedef uint8_t ui8_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ui8_t uint8$" } } */

uint8_t ui8_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _ui8_v1 _uint8_t$" } } */

ui8_t ui8_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ui8_v2 _ui8_t$" } } */

typedef uint16_t iu16_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _iu16_t uint16$" } } */

uint16_t iu16_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _iu16_v1 _uint16_t$" } } */

iu16_t iu16_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _iu16_v2 _iu16_t$" } } */

typedef uint32_t iu32_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _iu32_t uint32$" } } */

uint32_t iu32_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _iu32_v1 _uint32_t$" } } */

iu32_t iu32_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _iu32_v2 _iu32_t$" } } */

typedef uint64_t iu64_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _iu64_t uint64$" } } */

uint64_t iu64_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _iu64_v1 _uint64_t$" } } */

iu64_t iu64_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _iu64_v2 _iu64_t$" } } */

typedef const char cc_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _cc_t u?int8$" } } */

const char cc_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _cc_v1 u?int8$" } } */

cc_t cc_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _cc_v2 _cc_t$" } } */


/*** pointer and array types ***/
typedef void *vp_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _vp_t \\*byte$" } } */

void *vp_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _vp_v1 \\*byte$" } } */

vp_t vp_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _vp_v2 _vp_t$" } } */

typedef int **ipp_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ipp_t \\*\\*int\[0-9\]*$" } } */

int **ipp_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _ipp_v1 \\*\\*int\[0-9\]*$" } } */

ipp_t ipp_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ipp_v2 _ipp_t$" } } */

typedef char ca_t[];
/* { dg-final { scan-file godump-1.out "(?n)^type _ca_t \\\[0\\\]u?int8$" } } */

char ca_v1[]; /* { dg-warning "array 'ca_v1' assumed to have one element" } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ca_v1 \\\[0\\+1\\\]u?int8$" } } */

char ca_v1b[2];
/* { dg-final { scan-file godump-1.out "(?n)^var _ca_v1b \\\[1\\+1\\\]u?int8$" } } */

ca_t ca_v2; /* { dg-warning "array 'ca_v2' assumed to have one element" } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ca_v2 \\\[0\\+1\\\]u?int8$" } } */

typedef short sa2_t[2];
/* { dg-final { scan-file godump-1.out "(?n)^type _sa2_t \\\[1\\+1\\\]int\[0-9\]*$" } } */

short sa2_v1[2];
/* { dg-final { scan-file godump-1.out "(?n)^var _sa2_v1 \\\[1\\+1\\\]int\[0-9\]*$" } } */

sa2_t sa2_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sa2_v2 _sa2_t$" } } */


/*** floating point types ***/
typedef float f_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _f_t float\[0-9\]*$" } } */

float f_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _f_v1 float\[0-9\]*$" } } */

f_t f_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _f_v2 _f_t$" } } */

typedef double d_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _d_t float\[0-9\]*$" } } */

double d_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _d_v1 float\[0-9\]*$" } } */

d_t d_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _d_v2 _d_t$" } } */

typedef long double ld_t;
/* { dg-final { scan-file godump-1.out "(?n)^// type _ld_t INVALID-float-\[0-9\]*$" } } */

long double ld_v1;
/* { dg-final { scan-file godump-1.out "(?n)^// var _ld_v1 INVALID-float-\[0-9\]*$" } } */

ld_t ld_v2;
/* { dg-final { scan-file godump-1.out "(?n)^// var _ld_v2 INVALID-float-\[0-9\]*$" } } */

/*** complex types ***/
typedef _Complex cx_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _cx_t complex\[0-9\]*$" } } */

_Complex cx_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _cx_v1 complex\[0-9\]*$" } } */

cx_t cx_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _cx_v2 _cx_t$" } } */

typedef float _Complex fcx_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _fcx_t complex\[0-9\]*$" } } */

float _Complex fcx_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _fcx_v1 complex\[0-9\]*$" } } */

fcx_t fcx_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _fcx_v2 _fcx_t$" } } */

typedef double _Complex dcx_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _dcx_t complex\[0-9\]*$" } } */

double _Complex dcx_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _dcx_v1 complex\[0-9\]*$" } } */

dcx_t dcx_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _dcx_v2 _dcx_t$" } } */

typedef long double _Complex ldcx_t;
/* { dg-final { scan-file godump-1.out "(?n)^// type _ldcx_t INVALID-complex-\[0-9\]*$" } } */

long double _Complex ldcx_v1;
/* { dg-final { scan-file godump-1.out "(?n)^// var _ldcx_v1 INVALID-complex-\[0-9\]*$" } } */

ldcx_t ldcx_v2;
/* { dg-final { scan-file godump-1.out "(?n)^// var _ldcx_v2 INVALID-complex-\[0-9\]*$" } } */

typedef int _Complex icx_t;
/* { dg-final { scan-file godump-1.out "(?n)^// type _icx_t INVALID-complex-non-real$" } } */

int _Complex icx_v1;
/* { dg-final { scan-file godump-1.out "(?n)^// var _icx_v1 INVALID-complex-non-real$" } } */

icx_t icx_v2;
/* { dg-final { scan-file godump-1.out "(?n)^// var _icx_v2 INVALID-complex-non-real$" } } */


/*** nested typedefs ***/
typedef int32_t ni_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ni_t int32$" } } */

typedef ni_t ni2_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ni2_t int32$" } } */

ni2_t ni2_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ni2_v2 _ni2_t$" } } */

typedef ni2_t ni3_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _ni3_t int32$" } } */

ni3_t ni3_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _ni3_v2 _ni3_t$" } } */


/*** enums ***/
enum { E11 };
/* { dg-final { scan-file godump-1.out "(?n)^const _E11 = 0$" } } */

enum { EV11 } e1_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _e1_v1 u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EV11 = 0$" } } */

enum { E21, E22 };
/* { dg-final { scan-file godump-1.out "(?n)^const _E21 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _E22 = 1$" } } */

enum { EV21, EV22 } e2_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _e2_v1 u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EV21 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EV22 = 1$" } } */

enum { EN1 = 3, EN2 = 77, EN3 = -1, EN4 };
/* { dg-final { scan-file godump-1.out "(?n)^const _EN1 = 3$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EN2 = 77$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EN3 = -1$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EN4 = 0$" } } */

typedef enum { ET1, ET2 } et_t;
/* { dg-final { scan-file godump-1.out "(?n)^type _et_t u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ET1 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ET2 = 1$" } } */

typedef enum e_t_idem_v1 { ETIV1 } e_t_idem_v1;
/* { dg-final { scan-file godump-1.out "(?n)^type _e_t_idem_v1 u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ETIV1 = 0$" } } */

typedef enum e_t_idem_v2 e_t_idem_v2;
enum e_t_idem_v2 { ETIV2 };
/* { dg-final { scan-file godump-1.out "(?n)^type _e_t_idem_v2 u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ETIV2 = 0$" } } */

enum { ETV1, ETV2 } et_v1;
/* { dg-final { scan-file godump-1.out "(?n)^var _et_v1 u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ETV1 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ETV2 = 1$" } } */

et_t et_v2;
/* { dg-final { scan-file godump-1.out "(?n)^var _et_v2 _et_t$" } } */


/*** simple structs ***/
typedef struct { } ts0e;
/* { dg-final { scan-file godump-1.out "(?n)^type _ts0e struct \{ \}$" } } */

struct { } s0e;
/* { dg-final { scan-file godump-1.out "(?n)^var _s0e struct \{ \}$" } } */

typedef struct { int8_t e1; } ts1e;
/* { dg-final { scan-file godump-1.out "(?n)^type _ts1e struct \{ e1 int8; \}$" } } */

struct { int8_t e1; } s1e;
/* { dg-final { scan-file godump-1.out "(?n)^var _s1e struct \{ e1 int8; \}$" } } */

typedef struct { int8_t e1; void *e2; } ts2el;
/* { dg-final { scan-file godump-1.out "(?n)^type _ts2el struct \{ e1 int8; e2 \\*byte; \}$" } } */

struct { int8_t e1; void *e2; } s2el;
/* { dg-final { scan-file godump-1.out "(?n)^var _s2el struct \{ e1 int8; e2 \\*byte; \}$" } } */

typedef struct { void *e1; int8_t e2; } ts2eg;
/* { dg-final { scan-file godump-1.out "(?n)^type _ts2eg struct \{ e1 \\*byte; e2 int8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

struct { void *e1; int8_t e2; } s2eg;
/* { dg-final { scan-file godump-1.out "(?n)^var _s2eg struct \{ e1 \\*byte; e2 int8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

typedef struct { int64_t l; int8_t c; int32_t i; int16_t s; } tsme;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsme struct \{ l int64; c int8; i int32; s int16; Godump_0_pad \\\[.\\\]byte; \}$" } } */

struct { int64_t l; int8_t c; int32_t i; int16_t s; } sme;
/* { dg-final { scan-file godump-1.out "(?n)^var _sme struct \{ l int64; c int\8; i int32; s int16; Godump_0_pad \\\[.\\\]byte; \}$" } } */

typedef struct { int16_t sa[3]; int8_t ca[3]; } tsae;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsae struct \{ sa \\\[2\\+1\\\]int16; ca \\\[2\\+1\\\]int8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

struct { int16_t sa[3]; int8_t ca[3]; } sae;
/* { dg-final { scan-file godump-1.out "(?n)^var _sae struct \{ sa \\\[2\\+1\\\]int16; ca \\\[2\\+1\\\]int8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

typedef struct { float f; } tsf_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsf_equiv struct \{ f float\[0-9\]*; \}$" } } */

struct { float f; } sf_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^var _sf_equiv struct \{ f float\[0-9\]*; \}$" } } */

typedef struct { float f; uint8_t : 0; } tsf_not_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsf_not_equiv struct \{ f float\[0-9\]*; \}$" } } */

struct { float f; uint8_t : 0; } sf_not_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^var _sf_not_equiv struct \{ f float\[0-9\]*; \}$" } } */

typedef struct { double d; } tsd_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsd_equiv struct \{ d float\[0-9\]*; \}$" } } */

struct { double d; } sd_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^var _sd_equiv struct \{ d float\[0-9\]*; \}$" } } */

typedef struct { double d; uint8_t : 0; } tsd_not_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsd_not_equiv struct \{ d float\[0-9\]*; \}$" } } */

struct { double d; uint8_t : 0; } sd_not_equiv;
/* { dg-final { scan-file godump-1.out "(?n)^var _sd_not_equiv struct \{ d float\[0-9\]*; \}$" } } */

typedef struct s_undef_t s_undef_t2;

typedef struct s_fwd *s_fwd_p;
/* { dg-final { scan-file godump-1.out "(?n)^type _s_fwd_p \\*_s_fwd$" } } */

struct s_fwd v_fwd;
/* { dg-final { scan-file godump-1.out "(?n)^var _v_fwd _s_fwd" } } */

struct s_fwd { };
/* { dg-final { scan-file godump-1.out "(?n)^type _s_fwd struct \{ \}$" } } */

typedef struct s_t_idem_v1 {} s_t_idem_v1;
/* { dg-final { scan-file godump-1.out "(?n)^type _s_t_idem_v1 struct \{ \}$" } } */

typedef struct s_t_idem_v2 s_t_idem_v2;
struct s_t_idem_v2 { };
/* { dg-final { scan-file godump-1.out "(?n)^type _s_t_idem_v2 struct \{ \}$" } } */

/*** nested structs ***/
typedef struct { struct { uint8_t ca[3]; } s; uint32_t i; } tsn;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsn struct \{ s struct \{ ca \\\[2\\+1\\\]uint8; \}; i uint32; \}$" } } */

struct { struct { uint8_t ca[3]; } s; uint32_t i; } sn;
/* { dg-final { scan-file godump-1.out "(?n)^var _sn struct \{ s struct \{ ca \\\[2\\+1\\\]uint8; \}; i uint32; \}$" } } */

typedef struct { struct { uint8_t a; uint16_t s; }; uint8_t b; } tsn_anon;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsn_anon struct \{ a uint8; s uint16; b uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]int16; \}$" } } */

struct { struct { uint8_t a; uint16_t s; }; uint8_t b; } sn_anon;
/* { dg-final { scan-file godump-1.out "(?n)^var _sn_anon struct \{ a uint8; s uint16; b uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]int16; \}$" } } */


/*** structs with bitfields ***/
typedef struct { uint8_t : 0; uint8_t c; } tsbf_anon_pad1;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad1 struct \{ c uint8; \}$" } } */

struct { uint8_t : 0; uint8_t c; } sbf_anon_pad1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad1 struct \{ c uint8; \}$" } } */

typedef struct { uint8_t : 1; uint8_t c; } tsbf_anon_pad2;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

struct { uint8_t : 1; uint8_t c; } sbf_anon_pad2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

typedef struct { uint8_t : 7; uint8_t c; } tsbf_anon_pad3;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad3 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

struct { uint8_t : 7; uint8_t c; } sbf_anon_pad3;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad3 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

typedef struct { uint8_t : 8; uint8_t c; } tsbf_anon_pad4;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad4 struct \{ Godump_0 uint8; c uint8; \}$" } } */

struct { uint8_t : 8; uint8_t c; } sbf_anon_pad4;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad4 struct \{ Godump_0 uint8; c uint8; \}$" } } */

typedef struct { uint64_t : 0; uint8_t c; } tsbf_anon_pad5;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad5 struct \{ c uint8; \}$" } } */

struct { uint64_t : 0; uint8_t c; } sbf_anon_pad5;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad5 struct \{ c uint8; \}$" } } */

typedef struct { uint64_t : 1; uint8_t c; } tsbf_anon_pad6;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad6 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

struct { uint64_t : 1; uint8_t c; } sbf_anon_pad6;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad6 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

typedef struct { uint64_t : 63; uint8_t c; } tsbf_anon_pad7;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad7 struct \{ Godump_0_pad \\\[8\\\]byte; c uint8; \}$" } } */

struct { uint64_t : 63; uint8_t c; } sbf_anon_pad7;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad7 struct \{ Godump_0_pad \\\[8\\\]byte; c uint8; \}$" } } */

typedef struct { uint64_t : 64; uint8_t c; } tsbf_anon_pad8;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad8 struct \{ Godump_0 uint64; c uint8; \}$" } } */

struct { uint64_t : 64; uint8_t c; } sbf_anon_pad8;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad8 struct \{ Godump_0 uint64; c uint8; \}$" } } */

typedef struct { uint8_t bf : 1; uint8_t c; } tsbf_pad8_1;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad8_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

struct { uint8_t bf : 1; uint8_t c; } sbf_pad8_1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad8_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

typedef struct { uint8_t bf : 7; uint8_t c; } tsbf_pad8_2;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad8_2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

struct { uint8_t bf : 7; uint8_t c; } sbf_pad8_2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad8_2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; \}$" } } */

typedef struct { uint8_t bf : 8; uint8_t c; } tsbf_pad8_3;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad8_3 struct \{ bf uint8; c uint8; \}$" } } */

struct { uint8_t bf : 8; uint8_t c; } sbf_pad8_3;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad8_3 struct \{ bf uint8; c uint8; \}$" } } */

typedef struct { uint16_t bf : 1; uint8_t c; } tsbf_pad16_1;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad16_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; Godump_1_align \\\[0\\\]int16; \}$" } } */

struct { uint16_t bf : 1; uint8_t c; } sbf_pad16_1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad16_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; Godump_1_align \\\[0\\\]int16; \}$" } } */

typedef struct { uint16_t bf : 15; uint8_t c; } tsbf_pad16_2;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad16_2 struct \{ Godump_0_pad \\\[2\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int16; \}$" } } */

struct { uint16_t bf : 15; uint8_t c; } sbf_pad16_2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad16_2 struct \{ Godump_0_pad \\\[2\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int16; \}$" } } */

typedef struct { uint16_t bf : 16; uint8_t c; } tsbf_pad16_3;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad16_3 struct \{ bf uint16; c uint8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

struct { uint16_t bf : 16; uint8_t c; } sbf_pad16_3;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad16_3 struct \{ bf uint16; c uint8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

typedef struct { uint32_t bf : 1; uint8_t c; } tsbf_pad32_1;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad32_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int32; \}$" } } */

struct { uint32_t bf : 1; uint8_t c; } sbf_pad32_1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad32_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int32; \}$" } } */

typedef struct { uint32_t bf : 31; uint8_t c; } tsbf_pad32_2;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad32_2 struct \{ Godump_0_pad \\\[4\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int32; \}$" } } */

struct { uint32_t bf : 31; uint8_t c; } sbf_pad32_2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad32_2 struct \{ Godump_0_pad \\\[4\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int32; \}$" } } */

typedef struct { uint32_t bf : 32; uint8_t c; } tsbf_pad32_3;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad32_3 struct \{ bf uint32; c uint8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

struct { uint32_t bf : 32; uint8_t c; } sbf_pad32_3;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad32_3 struct \{ bf uint32; c uint8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

typedef struct { uint64_t bf : 1; uint8_t c; } tsbf_pad64_1;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad64_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int64; \}$" } } */

struct { uint64_t bf : 1; uint8_t c; } sbf_pad64_1;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad64_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int64; \}$" } } */

typedef struct { uint64_t bf : 63; uint8_t c; } tsbf_pad64_2;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad64_2 struct \{ Godump_0_pad \\\[8\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int64; \}$" } } */

struct { uint64_t bf : 63; uint8_t c; } sbf_pad64_2;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad64_2 struct \{ Godump_0_pad \\\[8\\\]byte; c uint8; Godump_1_pad \\\[.\\\]byte; Godump_2_align \\\[0\\\]int64; \}$" } } */

typedef struct { uint64_t bf : 64; uint8_t c; } tsbf_pad64_3;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad64_3 struct \{ bf uint\[0-9\]*; c uint8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

struct { uint64_t bf : 64; uint8_t c; } sbf_pad64_3;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad64_3 struct \{ bf uint\[0-9\]*; c uint8; Godump_0_pad \\\[.\\\]byte; \}$" } } */

typedef struct { uint8_t b1 : 1; } tsbf_1b;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_1b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */

struct { uint8_t b1 : 1; } sbf_1b;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_1b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */

typedef struct
{
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
} tsbf_8b;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_8b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */

struct
{
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
} sbf_8b;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_8b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */

typedef struct {
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
  uint8_t b9 : 1;
} tsbf_9b;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_9b struct \{ Godump_0_pad \\\[2\\\]byte; \}$" } } */

struct {
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
  uint8_t b9 : 1;
} sbf_9b;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_9b struct \{ Godump_0_pad \\\[2\\\]byte; \}$" } } */

typedef struct {
  uint8_t b1 : 7; uint8_t b2 : 7; uint8_t b3 : 2;
} tsbf_18b;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_18b struct \{ Godump_0_pad \\\[3\\\]byte; \}$" } } */

struct {
  uint8_t b1 : 7; uint8_t b2 : 7; uint8_t b3 : 2;
} sbf_18b;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_18b struct \{ Godump_0_pad \\\[3\\\]byte; \}$" } } */

struct
{
  uint16_t bf1 : 8;
  uint8_t c;
  uint16_t bf2 : 8;
  uint32_t bf3 : 12;
  uint16_t s;
} sbf_gaps;
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_gaps struct \{ bf1 uint8; c uint8; bf2 uint8; Godump_0_pad \\\[2\\\]byte; s uint16; Godump_1_align \\\[0\\\]int32; \}$" } } */

typedef struct
{
  uint16_t bf1 : 8;
  uint8_t c;
  uint16_t bf2 : 8;
  uint32_t bf3 : 12;
  uint16_t s;
} tsbf_gaps;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_gaps struct \{ bf1 uint8; c uint8; bf2 uint8; Godump_0_pad \\\[2\\\]byte; s uint16; Godump_1_align \\\[0\\\]int32; \}$" } } */

typedef struct
{
	union
	{
		int64_t : 1;
		union
		{
			int32_t bf : 1;
			union
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} ts_nested;
/* { dg-final { scan-file godump-1.out "(?n)^type _ts_nested struct \{ u struct \{ s int16; Godump_0_pad \\\[2\\\]byte; Godump_1_align \\\[0\\\]u?int32; \}; \}$" } } */

struct
{
	union
	{
		int64_t : 1;
		union
		{
			int32_t bf : 1;
			union
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} s_nested;
/* { dg-final { scan-file godump-1.out "(?n)^var _s_nested struct \{ u struct \{ s int16; Godump_0_pad \\\[2\\\]byte; Godump_1_align \\\[0\\\]u?int32; \}; \}$" } } */

typedef struct
{
	struct
	{
		int64_t : 1;
		struct
		{
			int32_t bf : 1;
			struct
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} ts_nested2;
/* { dg-final { scan-file godump-1.out "(?n)^type _ts_nested2 struct \{ u struct \{ Godump_0_pad \\\[4\\\]byte; Godump_1_pad \\\[2\\\]byte; s int16; c int8; Godump_2_pad \\\[1\\\]byte; Godump_3_pad \\\[2\\\]byte; Godump_4_align \\\[0\\\]u?int32; \}; \}$" } } */

struct
{
	struct
	{
		int64_t : 1;
		struct
		{
			int32_t bf : 1;
			struct
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} s_nested2;
/* { dg-final { scan-file godump-1.out "(?n)^var _s_nested2 struct \{ u struct \{ Godump_0_pad \\\[4\\\]byte; Godump_1_pad \\\[2\\\]byte; s int16; c int8; Godump_2_pad \\\[1\\\]byte; Godump_3_pad \\\[2\\\]byte; Godump_4_align \\\[0\\\]u?int32; \}; \}$" } } */


/*** unions ***/
typedef union { } tue;
/* { dg-final { scan-file godump-1.out "(?n)^type _tue struct \{ \}$" } } */

union { } ue;
/* { dg-final { scan-file godump-1.out "(?n)^var _ue struct \{ \}$" } } */

typedef union u_t_idem_v1 { } u_t_idem_v1;
/* { dg-final { scan-file godump-1.out "(?n)^type _u_t_idem_v1 struct \{ \}$" } } */

typedef union u_t_idem_v2 u_t_idem_v2;
union u_t_idem_v2 { };
/* { dg-final { scan-file godump-1.out "(?n)^type _u_t_idem_v2 struct \{ \}$" } } */

typedef union { uint8_t c; uint64_t l; } tu1;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu1 struct \{ c uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

union { uint8_t c; uint64_t l; } u1;
/* { dg-final { scan-file godump-1.out "(?n)^var _u1 struct \{ c uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

typedef union { uint64_t l; uint8_t c; } tu2;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu2 struct \{ l uint64; \}$" } } */

union { uint64_t l; uint8_t c; } u2;
/* { dg-final { scan-file godump-1.out "(?n)^var _u2 struct \{ l uint64; \}$" } } */

typedef union { uint64_t l[3]; uint8_t c; } tu3;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu3 struct \{ l \\\[2\\+1\\\]uint64; \}$" } } */

union { uint64_t l[3]; uint8_t c; } u3;
/* { dg-final { scan-file godump-1.out "(?n)^var _u3 struct \{ l \\\[2\\+1\\\]uint64; \}$" } } */

typedef struct { union { uint8_t c; uint64_t l; }; } tsu_anon;
/* { dg-final { scan-file godump-1.out "(?n)^type _tsu_anon struct \{ c uint8; Godump_0_pad \\\[7\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

struct { union { uint8_t c; uint64_t l; }; } su_anon;
/* { dg-final { scan-file godump-1.out "(?n)^var _su_anon struct \{ c uint8; Godump_0_pad \\\[7\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

typedef union { uint64_t bf : 1; uint8_t ca[5]; } tu_size;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu_size struct \{ ca \\\[4\\+1\\\]uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

union { uint64_t bf : 1; uint8_t ca[5]; } u_size;
/* { dg-final { scan-file godump-1.out "(?n)^var _u_size struct \{ ca \\\[4\\+1\\\]uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

typedef union { uint64_t : 1; uint8_t ca[5]; } tu2_size;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu2_size struct \{ ca \\\[4\\+1\\\]uint8; \}$" } } */

union { uint64_t : 1; uint8_t ca[5]; } u2_size;
/* { dg-final { scan-file godump-1.out "(?n)^var _u2_size struct \{ ca \\\[4\\+1\\\]uint8; \}$" } } */

typedef union u_undef_t u_undef_t2;

typedef union { uint64_t b : 1; uint8_t ca[5]; } tu3_size;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu3_size struct \{ ca \\\[4\\+1\\\]uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

union { uint64_t b : 1; uint8_t ca[5]; } u3_size;
/* { dg-final { scan-file godump-1.out "(?n)^var _u3_size struct \{ ca \\\[4\\+1\\\]uint8; Godump_0_pad \\\[.\\\]byte; Godump_1_align \\\[0\\\]u?int64; \}$" } } */

typedef union
{
	union
	{
		int64_t : 1;
		union
		{
			int32_t bf : 1;
			union
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} tu_nested;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu_nested struct \{ u struct \{ s int16; Godump_0_pad \\\[2\\\]byte; Godump_1_align \\\[0\\\]u?int32; \}; \}$" } } */

union
{
	union
	{
		int64_t : 1;
		union
		{
			int32_t bf : 1;
			union
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} u_nested;
/* { dg-final { scan-file godump-1.out "(?n)^var _u_nested struct \{ u struct \{ s int16; Godump_0_pad \\\[2\\\]byte; Godump_1_align \\\[0\\\]u?int32; \}; \}$" } } */

typedef union
{
	struct
	{
		int64_t : 1;
		struct
		{
			int32_t bf : 1;
			struct
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} tu_nested2;
/* { dg-final { scan-file godump-1.out "(?n)^type _tu_nested2 struct \{ u struct \{ Godump_0_pad \\\[4\\\]byte; Godump_1_pad \\\[2\\\]byte; s int16; c int8; Godump_2_pad \\\[1\\\]byte; Godump_3_pad \\\[2\\\]byte; Godump_4_align \\\[0\\\]u?int32; \}; \}$" } } */

union
{
	struct
	{
		int64_t : 1;
		struct
		{
			int32_t bf : 1;
			struct
			{
				int16_t s;
				int8_t c;
			};
		};
	} u;
} u_nested2;
/* { dg-final { scan-file godump-1.out "(?n)^var _u_nested2 struct \{ u struct \{ Godump_0_pad \\\[4\\\]byte; Godump_1_pad \\\[2\\\]byte; s int16; c int8; Godump_2_pad \\\[1\\\]byte; Godump_3_pad \\\[2\\\]byte; Godump_4_align \\\[0\\\]u?int32; \}; \}$" } } */


/*** functions ***/
extern uint32_t func1(uint8_t c);
/* { dg-final { scan-file godump-1.out "(?n)^func _func1 \[(\]uint\[0-9\]*\[)\] uint\[0-9\]* __asm__\[(\]\"func1\"\[)\]$" } } */

typedef int8_t (*func_t)(void *p);
/* { dg-final { scan-file godump-1.out "(?n)^type _func_t func\[(\]\\*byte\[)\] int\[0-9\]*$" } } */

/* { dg-final { remove-build-file "godump-1.out" } } */
