/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -fno-inline-functions" } */

/* Test simple extract/insert/slat operations.  Make sure all types are
   supported with various options.  */

#include <altivec.h>
int c0 = 0;
int c1 = 1;
int c3 = 3;
int c7 = 7;
int c15 = 15;

double extract_df_0_reg (vector double p) { return vec_extract (p, c0); }
double extract_df_1_reg (vector double p) { return vec_extract (p, c1); }
double extract_df_n_reg (vector double p, int n) { return vec_extract (p, n); }

double extract_df_0_mem (vector double *p) { return vec_extract (*p, c0); }
double extract_df_1_mem (vector double *p) { return vec_extract (*p, c1); }
double extract_df_n_mem (vector double *p, int n) { return vec_extract (*p, n); }

vector double insert_df_0 (vector double p, double x) { return vec_insert (x, p, c0); }
vector double insert_df_1 (vector double p, double x) { return vec_insert (x, p, c1); }
vector double insert_df_n (vector double p, double x, int n) { return vec_insert (x, p, n); }

vector double splat_df_reg (double x) { return vec_splats (x); }
vector double splat_df_mem (double *x) { return vec_splats (*x); }

#ifdef _ARCH_PPC64
#define ll long
#else
#define ll long long
#endif

ll extract_di_0_reg (vector ll p) { return vec_extract (p, c0); }
ll extract_di_1_reg (vector ll p) { return vec_extract (p, c1); }
ll extract_di_n_reg (vector ll p, int n) { return vec_extract (p, n); }

ll extract_di_0_mem (vector ll *p) { return vec_extract (*p, c0); }
ll extract_di_1_mem (vector ll *p) { return vec_extract (*p, c1); }
ll extract_di_n_mem (vector ll *p, int n) { return vec_extract (*p, n); }

vector ll insert_di_0 (vector ll p, ll x) { return vec_insert (x, p, c0); }
vector ll insert_di_1 (vector ll p, ll x) { return vec_insert (x, p, c1); }
vector ll insert_di_n (vector ll p, ll x, int n) { return vec_insert (x, p, n); }
vector ll insert_di_0_v2 (vector ll int p, ll int x) { return vec_insert (x, p, c0); }
vector unsigned ll insert_di_0_v3 (vector unsigned ll int p, unsigned ll int x) { return vec_insert (x, p, c0); }

vector ll splat_di_reg (ll x) { return vec_splats (x); }
vector ll splat_di_mem (ll *x) { return vec_splats (*x); }

float extract_sf_0_reg (vector float p) { return vec_extract (p, c0); }
float extract_sf_3_reg (vector float p) { return vec_extract (p, c3); }
float extract_sf_n_reg (vector float p, int n) { return vec_extract (p, n); }

float extract_sf_0_mem (vector float *p) { return vec_extract (*p, c0); }
float extract_sf_3_mem (vector float *p) { return vec_extract (*p, c3); }
float extract_sf_n_mem (vector float *p, int n) { return vec_extract (*p, n); }

vector float insert_sf_0 (vector float p, float x) { return vec_insert (x, p, c0); }
vector float insert_sf_3 (vector float p, float x) { return vec_insert (x, p, c3); }
vector float insert_sf_n (vector float p, float x, int n) { return vec_insert (x, p, n); }

vector float splat_sf_reg (float x) { return vec_splats (x); }
vector float splat_sf_mem (float *x) { return vec_splats (*x); }

int extract_si_0_reg (vector int p) { return vec_extract (p, c0); }
int extract_si_3_reg (vector int p) { return vec_extract (p, c3); }
int extract_si_n_reg (vector int p, int n) { return vec_extract (p, n); }

int extract_si_0_mem (vector int *p) { return vec_extract (*p, c0); }
int extract_si_3_mem (vector int *p) { return vec_extract (*p, c3); }
int extract_si_n_mem (vector int *p, int n) { return vec_extract (*p, n); }

vector int insert_si_0 (vector int p, int x) { return vec_insert (x, p, c0); }
vector int insert_si_3 (vector int p, int x) { return vec_insert (x, p, c3); }
vector int insert_si_n (vector int p, int x, int n) { return vec_insert (x, p, n); }

vector int splat_si_reg (int x) { return vec_splats (x); }
vector int splat_si_mem (int *x) { return vec_splats (*x); }

unsigned int extract_usi_0_reg (vector unsigned int p) { return vec_extract (p, c0); }
unsigned int extract_usi_3_reg (vector unsigned int p) { return vec_extract (p, c3); }
unsigned int extract_usi_n_reg (vector unsigned int p, int n) { return vec_extract (p, n); }

unsigned int extract_usi_0_mem (vector unsigned int *p) { return vec_extract (*p, c0); }
unsigned int extract_usi_3_mem (vector unsigned int *p) { return vec_extract (*p, c3); }
unsigned int extract_usi_n_mem (vector unsigned int *p, int n) { return vec_extract (*p, n); }

vector unsigned int insert_usi_0 (vector unsigned int p, unsigned int x) { return vec_insert (x, p, c0); }
vector unsigned int insert_usi_3 (vector unsigned int p, unsigned int x) { return vec_insert (x, p, c3); }
vector unsigned int insert_usi_n (vector unsigned int p, unsigned int x, int n) { return vec_insert (x, p, n); }

vector unsigned int splat_usi_reg (unsigned int x) { return vec_splats (x); }
vector unsigned int splat_usi_mem (unsigned int *x) { return vec_splats (*x); }

short extract_hi_0_reg (vector short p) { return vec_extract (p, c0); }
short extract_hi_7_reg (vector short p) { return vec_extract (p, c7); }
short extract_hi_n_reg (vector short p, int n) { return vec_extract (p, n); }

short extract_hi_0_mem (vector short *p) { return vec_extract (*p, c0); }
short extract_hi_7_mem (vector short *p) { return vec_extract (*p, c7); }
short extract_hi_n_mem (vector short *p, int n) { return vec_extract (*p, n); }

vector short insert_hi_0 (vector short p, short x) { return vec_insert (x, p, c0); }
vector short insert_hi_7 (vector short p, short x) { return vec_insert (x, p, c7); }
vector short insert_hi_n (vector short p, short x, int n) { return vec_insert (x, p, n); }

vector short splat_hi_reg (short x) { return vec_splats (x); }
vector short splat_hi_mem (short *x) { return vec_splats (*x); }

unsigned short extract_uhi_0_reg (vector unsigned short p) { return vec_extract (p, c0); }
unsigned short extract_uhi_7_reg (vector unsigned short p) { return vec_extract (p, c7); }
unsigned short extract_uhi_n_reg (vector unsigned short p, int n) { return vec_extract (p, n); }

unsigned short extract_uhi_0_mem (vector unsigned short *p) { return vec_extract (*p, c0); }
unsigned short extract_uhi_7_mem (vector unsigned short *p) { return vec_extract (*p, c7); }
unsigned short extract_uhi_n_mem (vector unsigned short *p, int n) { return vec_extract (*p, n); }

vector unsigned short insert_uhi_0 (vector unsigned short p, unsigned short x) { return vec_insert (x, p, c0); }
vector unsigned short insert_uhi_7 (vector unsigned short p, unsigned short x) { return vec_insert (x, p, c7); }
vector unsigned short insert_uhi_n (vector unsigned short p, unsigned short x, int n) { return vec_insert (x, p, n); }

vector unsigned short splat_uhi_reg (unsigned short x) { return vec_splats (x); }
vector unsigned short splat_uhi_mem (unsigned short *x) { return vec_splats (*x); }

signed char extract_qi_0_reg (vector signed char p) { return vec_extract (p, c0); }
unsigned char extract_uqi_0_reg (vector unsigned char p) { return vec_extract (p, c0); }
signed char extract_qi_1_reg5 (vector signed char p) { return vec_extract (p, c15); }
signed char extract_qi_n_reg (vector signed char p, int n) { return vec_extract (p, n); }

signed char extract_qi_0_mem (vector signed char *p) { return vec_extract (*p, c0); }
signed char extract_qi_1_mem5 (vector signed char *p) { return vec_extract (*p, c15); }
signed char extract_qi_n_mem (vector signed char *p, int n) { return vec_extract (*p, n); }

vector signed char insert_qi_0 (vector signed char p, signed char x) { return vec_insert (x, p, c0); }
vector signed char insert_qi_15 (vector signed char p, signed char x) { return vec_insert (x, p, c15); }
vector signed char insert_qi_n (vector signed char p, signed char x, int n) { return vec_insert (x, p, n); }

vector signed char splat_qi_reg (signed char x) { return vec_splats (x); }
vector signed char splat_qi_mem (signed char *x) { return vec_splats (*x); }

unsigned char extract_uqi_1_reg5 (vector unsigned char p) { return vec_extract (p, c15); }
unsigned char extract_uqi_n_reg (vector unsigned char p, int n) { return vec_extract (p, n); }

unsigned char extract_uqi_0_mem (vector unsigned char *p) { return vec_extract (*p, c0); }
unsigned char extract_uqi_1_mem5 (vector unsigned char *p) { return vec_extract (*p, c15); }
unsigned char extract_uqi_n_mem (vector unsigned char *p, int n) { return vec_extract (*p, n); }

vector unsigned char insert_uqi_0 (vector unsigned char p, unsigned char x) { return vec_insert (x, p, c0); }
vector unsigned char insert_uqi_15 (vector unsigned char p, unsigned char x) { return vec_insert (x, p, c15); }
vector unsigned char insert_uqi_n (vector unsigned char p, unsigned char x, int n) { return vec_insert (x, p, n); }

vector unsigned char splat_uqi_reg (unsigned char x) { return vec_splats (x); }
vector unsigned char splat_uqi_mem (unsigned char *x) { return vec_splats (*x); }

vector signed char splat_sc_s8 () { return vec_splat_s8 (2); }
vector unsigned char splat_uc_u8 () { return vec_splat_u8 (3); }

vector signed short int splat_ssi_s16() { return vec_splat_s16 (4); }
vector unsigned short int splat_usi_s16() { return vec_splat_u16 (5); }

vector signed int splat_si_s32() { return vec_splat_s32 (6); }
vector unsigned int splat_ui_u32() { return vec_splat_u32 (7); }

vector signed long long splat_sll (signed long long x)
  { return vec_splats (x); }

vector unsigned long long splat_uc (unsigned long long x)
  { return vec_splats (x); }

#ifdef __SIZEOF_INT128__
/* Note, int128 not supported on 32-bit platforms.  */
vector signed __int128 splat_int128 (signed __int128 x) { return vec_splats (x); }
vector unsigned __int128 splat_uint128 (unsigned __int128 x) { return vec_splats (x); }
#endif

/* Expected results:
   vec_extract                       rldic
   vec_insert                        rldicr
   vec_splats                        xxspltd detected as xxpermdi
   vec_splat_s8                      vspltisb
   vec_splat_u8                      vspltisb
   vec_splat_s16                     vspltish
   vec_splat_u16                     vspltish
   vec_splat_s32                     vspltisw
   vec_splat_u32                     vspltisw
   return 128 vec_splats             vspltisw
 */

/* { dg-final { scan-assembler-times {\mrldic\M} 0  { target { be && ilp32 } } } } */
/* { dg-final { scan-assembler-times {\mrldic\M} 64 { target { be && lp64 } } } } */
/* { dg-final { scan-assembler-times {\mrldic\M} 64 { target le } } } */
/* { dg-final { scan-assembler-times "xxpermdi" 4 { target be } } } */
/* { dg-final { scan-assembler-times "xxpermdi" 6 { target le } } } */
/* { dg-final { scan-assembler-times "vspltisb" 2 } } */
/* { dg-final { scan-assembler-times "vspltish" 2 } } */
/* { dg-final { scan-assembler-times "vspltisw" 2 } } */
