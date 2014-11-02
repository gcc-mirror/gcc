/* Test -fdump-go-specs option.  */

/* { dg-options "-c -fdump-go-spec=godump-1.out" } */
/* { dg-do compile } */
/* { dg-skip-if "not supported for target" { ! "s390*-*-* i?86-*-* x86_64-*-*" } } */
/* { dg-skip-if "not supported for target" { ! lp64 } } */

#include <stdint.h>

/* integer based types */
typedef char c_t;
char c_v1;
c_t c_v2;
typedef short s_t;
short s_v1;
s_t s_v2;
typedef int i_t;
int i_v1;
i_t i_v2;
typedef long l_t;
long l_v1;
l_t l_v2;
typedef long long ll_t;
long long ll_v1;
ll_t ll_v2;
typedef unsigned char uc_t;
unsigned char uc_v1;
uc_t uc_v2;
typedef unsigned short us_t;
unsigned short us_v1;
us_t us_v2;
typedef unsigned int ui_t;
unsigned int ui_v1;
ui_t ui_v2;
typedef unsigned long ul_t;
unsigned long ul_v1;
ul_t ul_v2;
typedef unsigned long long ull_t;
unsigned long long ull_v1;
ull_t ull_v2;
typedef signed char sc_t;
signed char sc_v1;
sc_t sc_v2;
typedef signed short ss_t;
signed short ss_v1;
ss_t ss_v2;
typedef signed int si_t;
signed int si_v1;
si_t si_v2;
typedef signed long sl_t;
signed long sl_v1;
sl_t sl_v2;
typedef signed long long sll_t;
signed long long sll_v1;
sll_t sll_v2;
typedef int8_t i8_t;
int8_t i8_v1;
i8_t i8_v2;
typedef int16_t i16_t;
int16_t i16_v1;
i16_t i16_v2;
typedef int32_t i32_t;
int32_t i32_v1;
i32_t i32_v2;
typedef int64_t i64_t;
int64_t i64_v1;
i64_t i64_v2;
typedef uint8_t ui8_t;
uint8_t ui8_v1;
ui8_t ui8_v2;
typedef uint16_t iu16_t;
uint16_t iu16_v1;
iu16_t iu16_v2;
typedef uint32_t iu32_t;
uint32_t iu32_v1;
iu32_t iu32_v2;
typedef uint64_t iu64_t;
uint64_t iu64_v1;
iu64_t iu64_v2;
typedef const char cc_t;
const char cc_v1;
cc_t cc_v2;

/* pointer and array types */
typedef void *vp_t;
void *vp_v1;
vp_t vp_v2;
typedef int **ipp_t;
int **ipp_v1;
ipp_t ipp_v2;
typedef char ca_t[];
char ca_v1[]; /* { dg-warning "array 'ca_v1' assumed to have one element" } */
char ca_v1b[2];
ca_t ca_v2; /* { dg-warning "array 'ca_v2' assumed to have one element" } */
typedef short sa2_t[2];
short sa2_v1[2];
sa2_t sa2_v2;

/* floating point types */
typedef float f_t;
float f_v1;
f_t f_v2;
typedef double d_t;
double d_v1;
d_t d_v2;
typedef long double ld_t;
long double ld_v1;
ld_t ld_v2;
typedef _Complex cx_t;
_Complex cx_v1;
cx_t cx_v2;
typedef float _Complex fcx_t;
float _Complex fcx_v1;
fcx_t fcx_v2;
typedef double _Complex dcx_t;
double _Complex dcx_v1;
dcx_t dcx_v2;
typedef long double _Complex ldcx_t;
long double _Complex ldcx_v1;
ldcx_t ldcx_v2;
typedef int _Complex icx_t;
int _Complex icx_v1;
icx_t icx_v2;

/* nested typedefs */
typedef int ni_t;
typedef ni_t ni2_t;
ni2_t ni2_v2;
typedef ni2_t ni3_t;
ni3_t ni3_v2;

/* enums */
enum { E11 };
enum { EV11 } e1_v1;
enum { E21, E22 };
enum { EV21, EV22 } e2_v1;
enum { EN1 = 3, EN2 = 77, EN3 = -1, EN4 };
typedef enum { ET1, ET2 } et_t;
enum { ETV1, ETV2 } et_v1;
et_t et_v2;

/* simple structs */
typedef struct { } ts0e;
struct { } s0e;
typedef struct { int8_t e1; } ts1e;
struct { int8_t e1; } s1e;
typedef struct { int8_t e1; void *e2; } ts2el;
struct { int8_t e1; void *e2; } s2el;
typedef struct { void *e1; int8_t e2; } ts2eg;
struct { void *e1; int8_t e2; } s2eg;
typedef struct { int64_t l; int8_t c; int32_t i; int16_t s; } tsme;
struct { int64_t l; int8_t c; int32_t i; int16_t s; } sme;
typedef struct { int16_t sa[3]; int8_t ca[3]; } tsae;
struct { int16_t sa[3]; int8_t ca[3]; } sae;
typedef struct { float f; } tsf_equiv;
struct { float f; } sf_equiv;
typedef struct { float f; uint8_t : 0; } tsf_not_equiv;
struct { float f; uint8_t : 0; } sf_not_equiv;
typedef struct { double d; } tsd_equiv;
struct { double d; } sd_equiv;
typedef struct { double d; uint8_t : 0; } tsd_not_equiv;
struct { double d; uint8_t : 0; } sd_not_equiv;
typedef struct s_undef_t s_undef_t2;

/* nested structs */
typedef struct { struct { uint8_t ca[3]; } s; uint32_t i; } tsn;
struct { struct { uint8_t ca[3]; } s; uint32_t i; } sn;
typedef struct { struct { uint8_t a; uint16_t s; }; uint8_t b; } tsn_anon;
struct { struct { uint8_t a; uint16_t s; }; uint8_t b; } sn_anon;

/* structs with bitfields */
typedef struct { uint8_t : 0; uint8_t c; } tsbf_anon_pad1;
struct { uint8_t : 0; uint8_t c; } sbf_anon_pad1;
typedef struct { uint8_t : 1; uint8_t c; } tsbf_anon_pad2;
struct { uint8_t : 1; uint8_t c; } sbf_anon_pad2;
typedef struct { uint8_t : 7; uint8_t c; } tsbf_anon_pad3;
struct { uint8_t : 7; uint8_t c; } sbf_anon_pad3;
typedef struct { uint8_t : 8; uint8_t c; } tsbf_anon_pad4;
struct { uint8_t : 8; uint8_t c; } sbf_anon_pad4;
typedef struct { uint64_t : 0; uint8_t c; } tsbf_anon_pad5;
struct { uint64_t : 0; uint8_t c; } sbf_anon_pad5;
typedef struct { uint64_t : 1; uint8_t c; } tsbf_anon_pad6;
struct { uint64_t : 1; uint8_t c; } sbf_anon_pad6;
typedef struct { uint64_t : 63; uint8_t c; } tsbf_anon_pad7;
struct { uint64_t : 63; uint8_t c; } sbf_anon_pad7;
typedef struct { uint64_t : 64; uint8_t c; } tsbf_anon_pad8;
struct { uint64_t : 64; uint8_t c; } sbf_anon_pad8;
typedef struct { uint8_t bf : 1; uint8_t c; } tsbf_pad8_1;
struct { uint8_t bf : 1; uint8_t c; } sbf_pad8_1;
typedef struct { uint8_t bf : 7; uint8_t c; } tsbf_pad8_2;
struct { uint8_t bf : 7; uint8_t c; } sbf_pad8_2;
typedef struct { uint8_t bf : 8; uint8_t c; } tsbf_pad8_3;
struct { uint8_t bf : 8; uint8_t c; } sbf_pad8_3;
typedef struct { uint16_t bf : 1; uint8_t c; } tsbf_pad16_1;
struct { uint16_t bf : 1; uint8_t c; } sbf_pad16_1;
typedef struct { uint16_t bf : 15; uint8_t c; } tsbf_pad16_2;
struct { uint16_t bf : 15; uint8_t c; } sbf_pad16_2;
typedef struct { uint16_t bf : 16; uint8_t c; } tsbf_pad16_3;
struct { uint16_t bf : 16; uint8_t c; } sbf_pad16_3;
typedef struct { uint32_t bf : 1; uint8_t c; } tsbf_pad32_1;
struct { uint32_t bf : 1; uint8_t c; } sbf_pad32_1;
typedef struct { uint32_t bf : 31; uint8_t c; } tsbf_pad32_2;
struct { uint32_t bf : 31; uint8_t c; } sbf_pad32_2;
typedef struct { uint32_t bf : 32; uint8_t c; } tsbf_pad32_3;
struct { uint32_t bf : 32; uint8_t c; } sbf_pad32_3;
typedef struct { uint64_t bf : 1; uint8_t c; } tsbf_pad64_1;
struct { uint64_t bf : 1; uint8_t c; } sbf_pad64_1;
typedef struct { uint64_t bf : 63; uint8_t c; } tsbf_pad64_2;
struct { uint64_t bf : 63; uint8_t c; } sbf_pad64_2;
typedef struct { uint64_t bf : 64; uint8_t c; } tsbf_pad64_3;
struct { uint64_t bf : 64; uint8_t c; } sbf_pad64_3;
typedef struct { uint8_t b1 : 1; } tsbf_1b;
struct { uint8_t b1 : 1; } sbf_1b;
typedef struct
{
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
} tsbf_8b;
struct
{
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
} sbf_8b;
typedef struct {
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
  uint8_t b9 : 1;
} tsbf_9b;
struct {
  uint8_t b1 : 1; uint8_t b2 : 1; uint8_t b3 : 1; uint8_t b4 : 1;
  uint8_t b5 : 1; uint8_t b6 : 1; uint8_t b7 : 1; uint8_t b8 : 1;
  uint8_t b9 : 1;
} sbf_9b;
typedef struct {
  uint8_t b1 : 7; uint8_t b2 : 7; uint8_t b3 : 2;
} tsbf_18b;
struct {
  uint8_t b1 : 7; uint8_t b2 : 7; uint8_t b3 : 2;
} sbf_18b;
struct
{
  uint16_t bf1 : 8;
  uint8_t c;
  uint16_t bf2 : 8;
  uint32_t bf3 : 12;
  uint16_t s;
} sbf_gaps;
typedef struct
{
  uint16_t bf1 : 8;
  uint8_t c;
  uint16_t bf2 : 8;
  uint32_t bf3 : 12;
  uint16_t s;
} tsbf_gaps;

/* unions */
typedef union { } tue;
union { } ue;
typedef union { uint8_t c; uint64_t l; } tu1;
union { uint8_t c; uint64_t l; } u1;
typedef union { uint64_t l; uint8_t c; } tu2;
union { uint64_t l; uint8_t c; } u2;
typedef union { uint64_t l[3]; uint8_t c; } tu3;
union { uint64_t l[3]; uint8_t c; } u3;
typedef struct { union { uint8_t c; uint64_t l; }; } tsu_anon;
struct { union { uint8_t c; uint64_t l; }; } su_anon;
typedef union { uint64_t bf : 1; uint8_t ca[5]; } tu_size;
union { uint64_t bf : 1; uint8_t ca[5]; } u_size;
typedef union { uint64_t : 1; uint8_t ca[5]; } tu2_size;
union { uint64_t : 1; uint8_t ca[5]; } u2_size;
typedef union u_undef_t u_undef_t2;
typedef union { uint64_t b : 1; uint8_t ca[5]; } tu3_size;
union { uint64_t b : 1; uint8_t ca[5]; } u3_size;

/* functions */
extern uint32_t func1(uint8_t c);
typedef int8_t (*func_t)(void *p);

/* Necessary quoting in the regexp patters:

     (?n) at beginning of pattern to make ^ and $ work.
     "     ->  \"
     *, +  ->  "*", "+"
     [, ]  ->  "\[", "\]"
     (, )  ->  "\[(\]", "\[)\]"
     {, }  ->  "\{", "\}"
*/

/* { dg-final { scan-file godump-1.out "(?n)^type _c_t u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _s_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _i_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _l_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ll_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _uc_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _us_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ui_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ul_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ull_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _sc_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ss_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _si_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _sl_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _sll_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _i8_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _i16_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _i32_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _i64_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ui8_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _iu16_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _iu32_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _iu64_t uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _cc_t u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _vp_t \\*byte$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ipp_t \\*\\*int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ca_t \\\[0\\\]u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _sa2_t \\\[1\\+1\\\]int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _f_t float\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _d_t float\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// type _ld_t INVALID-float-\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _cx_t complex\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _fcx_t complex\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _dcx_t complex\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// type _ldcx_t INVALID-complex-\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// type _icx_t INVALID-complex-non-real$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ni_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ni2_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ni3_t int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _et_t int$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ts0e struct \{ \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ts1e struct \{ e1 int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ts2el struct \{ e1 int\[0-9\]*; e2 \\*byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _ts2eg struct \{ e1 \\*byte; e2 int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsme struct \{ l int\[0-9\]*; c int\[0-9\]*; i int\[0-9\]*; s int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsae struct \{ sa \\\[2\\+1\\\]int\[0-9\]*; ca \\\[2\\+1\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsf_equiv struct \{ f float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsf_not_equiv struct \{ f float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsd_equiv struct \{ d float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsd_not_equiv struct \{ d float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsn struct \{ s struct \{ ca \\\[2\\+1\\\]uint\[0-9\]*; \}; i uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsn_anon struct \{ Godump_0 struct \{ a uint\[0-9\]*; s uint\[0-9\]*; \}; b uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad1 struct \{ c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad3 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad4 struct \{ Godump_0 uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad5 struct \{ c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad6 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad7 struct \{ Godump_0_pad \\\[8\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_anon_pad8 struct \{ Godump_0 uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad8_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad8_2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad8_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad16_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad16_2 struct \{ Godump_0_pad \\\[2\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad16_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad32_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad32_2 struct \{ Godump_0_pad \\\[4\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad32_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad64_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad64_2 struct \{ Godump_0_pad \\\[8\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_pad64_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_1b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_8b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_9b struct \{ Godump_0_pad \\\[2\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_18b struct \{ Godump_0_pad \\\[3\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsbf_gaps struct \{ bf1 uint\[0-9\]*; c uint\[0-9\]*; bf2 uint\[0-9\]*; Godump_0_pad \\\[2\\\]byte; s uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tue struct \{ Godump_0 \\\[0\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tu1 struct \{ c \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tu2 struct \{ l \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tu3 struct \{ l \\\[24\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tsu_anon struct \{ Godump_0 struct \{ c \\\[8\\\]byte; Godump_1_align \\\[0\\\]uint64; \}; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tu_size struct \{ bf \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tu2_size struct \{ Godump_0 \\\[5\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _tu3_size struct \{ b \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^type _func_t func\[(\]\\*byte\[)\] int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _c_v1 u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _c_v2 _c_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _s_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _s_v2 _s_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i_v2 _i_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _l_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _l_v2 _l_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ll_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ll_v2 _ll_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _uc_v1 uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _uc_v2 _uc_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _us_v1 uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _us_v2 _us_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ui_v1 uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ui_v2 _ui_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ul_v1 uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ul_v2 _ul_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ull_v1 uint\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ull_v2 _ull_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sc_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sc_v2 _sc_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ss_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ss_v2 _ss_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _si_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _si_v2 _si_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sl_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sl_v2 _sl_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sll_v1 int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sll_v2 _sll_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i8_v1 _int8_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i8_v2 _i8_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i16_v1 _int16_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i16_v2 _i16_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i32_v1 _int32_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i32_v2 _i32_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i64_v1 _int64_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _i64_v2 _i64_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ui8_v1 _uint8_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ui8_v2 _ui8_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _iu16_v1 _uint16_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _iu16_v2 _iu16_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _iu32_v1 _uint32_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _iu32_v2 _iu32_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _iu64_v1 _uint64_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _iu64_v2 _iu64_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _cc_v1 u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _cc_v2 _cc_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _vp_v1 \\*byte$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _vp_v2 _vp_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ipp_v1 \\*\\*int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ipp_v2 _ipp_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ca_v1 \\\[0\\+1\\\]u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ca_v1b \\\[1\\+1\\\]u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ca_v2 \\\[0\\+1\\\]u?int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sa2_v1 \\\[1\\+1\\\]int\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sa2_v2 _sa2_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _f_v1 float\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _f_v2 _f_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _d_v1 float\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _d_v2 _d_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// var _ld_v1 INVALID-float-\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// var _ld_v2 INVALID-float-\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _cx_v1 complex\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _cx_v2 _cx_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _fcx_v1 complex\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _fcx_v2 _fcx_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _dcx_v1 complex\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _dcx_v2 _dcx_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// var _ldcx_v1 INVALID-complex-\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// var _ldcx_v2 INVALID-complex-\[0-9\]*$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// var _icx_v1 INVALID-complex-non-real$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^// var _icx_v2 INVALID-complex-non-real$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ni2_v2 _ni2_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ni3_v2 _ni3_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _e1_v1 int$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _e2_v1 int$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _et_v1 int$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _et_v2 _et_t$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _s0e struct \{ \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _s1e struct \{ e1 int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _s2el struct \{ e1 int\[0-9\]*; e2 \\*byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _s2eg struct \{ e1 \\*byte; e2 int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sme struct \{ l int\[0-9\]*; c int\[0-9\]*; i int\[0-9\]*; s int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sae struct \{ sa \\\[2\\+1\\\]int\[0-9\]*; ca \\\[2\\+1\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sf_equiv struct \{ f float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sf_not_equiv struct \{ f float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sd_equiv struct \{ d float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sd_not_equiv struct \{ d float\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sn struct \{ s struct \{ ca \\\[2\\+1\\\]uint\[0-9\]*; \}; i uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sn_anon struct \{ Godump_0 struct \{ a uint\[0-9\]*; s uint\[0-9\]*; \}; b uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad1 struct \{ c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad3 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad4 struct \{ Godump_0 uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad5 struct \{ c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad6 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad7 struct \{ Godump_0_pad \\\[8\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_anon_pad8 struct \{ Godump_0 uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad8_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad8_2 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad8_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad16_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad16_2 struct \{ Godump_0_pad \\\[2\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad16_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad32_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad32_2 struct \{ Godump_0_pad \\\[4\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad32_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad64_1 struct \{ Godump_0_pad \\\[1\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad64_2 struct \{ Godump_0_pad \\\[8\\\]byte; c uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_pad64_3 struct \{ bf uint\[0-9\]*; c uint\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_1b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_8b struct \{ Godump_0_pad \\\[1\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_9b struct \{ Godump_0_pad \\\[2\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_18b struct \{ Godump_0_pad \\\[3\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _sbf_gaps struct \{ bf1 uint\[0-9\]*; c uint\[0-9\]*; bf2 uint\[0-9\]*; Godump_0_pad \\\[2\\\]byte; s uint\[0-9\]*; Godump_1_align \\\[0\\\]int\[0-9\]*; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _ue struct \{ Godump_0 \\\[0\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _u1 struct \{ c \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _u2 struct \{ l \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _u3 struct \{ l \\\[24\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _su_anon struct \{ Godump_0 struct \{ c \\\[8\\\]byte; Godump_1_align \\\[0\\\]uint64; \}; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _u_size struct \{ bf \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _u2_size struct \{ Godump_0 \\\[5\\\]byte; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^var _u3_size struct \{ b \\\[8\\\]byte; Godump_0_align \\\[0\\\]uint64; \}$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^func _func1 \[(\]uint\[0-9\]*\[)\] uint\[0-9\]* __asm__\[(\]\"func1\"\[)\]$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _E11 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _E21 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _E22 = 1$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EN1 = 3$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EN2 = 77$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EN3 = -1$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EN4 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ET1 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ET2 = 1$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ETV1 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _ETV2 = 1$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EV11 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EV21 = 0$" } } */
/* { dg-final { scan-file godump-1.out "(?n)^const _EV22 = 1$" } } */
