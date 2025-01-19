/* { dg-do compile } */
/* { dg-options "-mlsx -O3" } */

#define N 128

char c[N];
short int h[N];
int s[N];
long l[N];
float f[N];
double d[N];
unsigned char uc[N];
unsigned short int uh[N];
unsigned int us[N];
unsigned long ul[N];

/* { dg-final { scan-assembler-not "test_vec_pack_sfix_trunc_v2df:.*\tvftintrz\\.l\\.d.*-test_vec_pack_sfix_trunc_v2df\n" } } */
/* { dg-final { scan-assembler-not "test_vec_pack_sfix_trunc_v2df:.*\tvpickev\\.w.*-test_vec_pack_sfix_trunc_v2df\n" } } */
/* { dg-final { scan-assembler "test_vec_pack_sfix_trunc_v2df:.*\tvftintrz\\.w\\.d.*-test_vec_pack_sfix_trunc_v2df\n" } } */
void
test_vec_pack_sfix_trunc_v2df (void)
{
  for (int i = 0; i < N; i++)
    s[i] = d[i];
}

/* { dg-final { scan-assembler-not "test_vec_packs_float_v2di:.*\tmovgr2fr\\.d.*-test_vec_packs_float_v2di" } } */
/* { dg-final { scan-assembler "test_vec_packs_float_v2di:.*\tvffint\\.s\\.l.*-test_vec_packs_float_v2di" } } */
void
test_vec_packs_float_v2di (void)
{
  for (int i = 0; i < N; i++)
    f[i] = l[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpack_sfix_trunc_hi_lo_v4sf:.*\tftintrz\\.l\\.s.*-test_vec_unpack_sfix_trunc_hi_lo_v4sf" } } */
/* { dg-final { scan-assembler "test_vec_unpack_sfix_trunc_hi_lo_v4sf:.*\tvftintrzh\\.l\\.s.*-test_vec_unpack_sfix_trunc_hi_lo_v4sf" } } */
/* { dg-final { scan-assembler "test_vec_unpack_sfix_trunc_hi_lo_v4sf:.*\tvftintrzl\\.l\\.s.*-test_vec_unpack_sfix_trunc_hi_lo_v4sf" } } */
void
test_vec_unpack_sfix_trunc_hi_lo_v4sf (void)
{
  for (int i = 0; i < N; i++)
    l[i] = f[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpacks_float_hi_lo_v4si:.*\tvslti\\.w.*-test_vec_unpacks_float_hi_lo_v4si" } } */
/* { dg-final { scan-assembler-not "test_vec_unpacks_float_hi_lo_v4si:.*\tvilvl\\.w.*-test_vec_unpacks_float_hi_lo_v4si" } } */
/* { dg-final { scan-assembler-not "test_vec_unpacks_float_hi_lo_v4si:.*\tvilvh\\.w.*-test_vec_unpacks_float_hi_lo_v4si" } } */
/* { dg-final { scan-assembler-not "test_vec_unpacks_float_hi_lo_v4si:.*\tvffint\\.d\\.l.*-test_vec_unpacks_float_hi_lo_v4si" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_float_hi_lo_v4si:.*\tvffinth\\.d\\.w.*-test_vec_unpacks_float_hi_lo_v4si" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_float_hi_lo_v4si:.*\tvffintl\\.d\\.w.*-test_vec_unpacks_float_hi_lo_v4si" } } */
void
test_vec_unpacks_float_hi_lo_v4si (void)
{
  for (int i = 0; i < N; i++)
    d[i] = s[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpacks_hi_lo_v4si:.*\tvilvh\\.w.*-test_vec_unpacks_hi_lo_v4si" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v4si:.*\tvexth\\.d\\.w.*-test_vec_unpacks_hi_lo_v4si" } } */
void
test_vec_unpacks_hi_lo_v4si (void)
{
  for (int i = 0; i < N; i++)
    l[i] = s[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpacks_hi_lo_v8hi:.*\tvilvh\\.h.*-test_vec_unpacks_hi_lo_v8hi" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v8hi:.*\tvexth\\.w\\.h.*-test_vec_unpacks_hi_lo_v8hi" } } */
void
test_vec_unpacks_hi_lo_v8hi (void)
{
  for (int i = 0; i < N; i++)
    s[i] = h[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpacks_hi_lo_v16qi:.*\tvilvh\\.b.*-test_vec_unpacks_hi_lo_v16qi" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v16qi:.*\tvexth\\.h\\.b.*-test_vec_unpacks_hi_lo_v16qi" } } */
void
test_vec_unpacks_hi_lo_v16qi (void)
{
  for (int i = 0; i < N; i++)
    h[i] = c[i];
}

/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v4sf:.*\tvfcvtl\\.d\\.s.*-test_vec_unpacks_hi_lo_v4sf" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v4sf:.*\tvfcvth\\.d\\.s.*-test_vec_unpacks_hi_lo_v4sf" } } */
void
test_vec_unpacks_hi_lo_v4sf (void)
{
  for (int i = 0; i < N; i++)
    d[i] = f[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpacku_hi_lo_v4si:.*\tvilvh\\.w.*-test_vec_unpacku_hi_lo_v4si" } } */
/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v4si:.*\tvexth\\.du\\.wu.*-test_vec_unpacku_hi_lo_v4si" } } */
void
test_vec_unpacku_hi_lo_v4si (void)
{
  for (int i = 0; i < N; i++)
    ul[i] = us[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpacku_hi_lo_v8hi:.*\tvilvh\\.h.*-test_vec_unpacku_hi_lo_v8hi" } } */
/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v8hi:.*\tvexth\\.wu\\.hu.*-test_vec_unpacku_hi_lo_v8hi" } } */
void
test_vec_unpacku_hi_lo_v8hi (void)
{
  for (int i = 0; i < N; i++)
    us[i] = uh[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpacku_hi_lo_v16qi:.*\tvilvh\\.b.*-test_vec_unpacku_hi_lo_v16qi" } } */
/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v16qi:.*\tvexth\\.hu\\.bu.*-test_vec_unpacku_hi_lo_v16qi" } } */
void
test_vec_unpacku_hi_lo_v16qi (void)
{
  for (int i = 0; i < N; i++)
    uh[i] = uc[i];
}
