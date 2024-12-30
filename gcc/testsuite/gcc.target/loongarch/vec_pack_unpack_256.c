/* { dg-do compile } */
/* { dg-options "-mlasx -O3" } */

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

/* { dg-final { scan-assembler-not "test_vec_pack_sfix_trunc_v4df:.*\txvftintrz\\.l\\.d.*-test_vec_pack_sfix_trunc_v4df\n" } } */
/* { dg-final { scan-assembler-not "test_vec_pack_sfix_trunc_v4df:.*\txvpickev\\.w.*-test_vec_pack_sfix_trunc_v4df\n" } } */
/* { dg-final { scan-assembler "test_vec_pack_sfix_trunc_v4df:.*\txvftintrz\\.w\\.d.*-test_vec_pack_sfix_trunc_v4df\n" } } */
void
test_vec_pack_sfix_trunc_v4df (void)
{
  for (int i = 0; i < N; i++)
    s[i] = d[i];
}

/* { dg-final { scan-assembler-not "test_vec_packs_float_v4di:.*\tmovgr2fr\\.d.*-test_vec_packs_float_v4di" } } */
/* { dg-final { scan-assembler "test_vec_packs_float_v4di:.*\txvffint\\.s\\.l.*-test_vec_packs_float_v4di" } } */
void
test_vec_packs_float_v4di (void)
{
  for (int i = 0; i < N; i++)
    f[i] = l[i];
}

/* { dg-final { scan-assembler-not "test_vec_unpack_sfix_trunc_hi_lo_v8sf:.*\tftintrz\\.l\\.s.*-test_vec_unpack_sfix_trunc_hi_lo_v8sf" } } */
/* { dg-final { scan-assembler "test_vec_unpack_sfix_trunc_hi_lo_v8sf:.*\txvftintrzh\\.l\\.s.*-test_vec_unpack_sfix_trunc_hi_lo_v8sf" } } */
/* { dg-final { scan-assembler "test_vec_unpack_sfix_trunc_hi_lo_v8sf:.*\txvftintrzl\\.l\\.s.*-test_vec_unpack_sfix_trunc_hi_lo_v8sf" } } */
void
test_vec_unpack_sfix_trunc_hi_lo_v8sf (void)
{
  for (int i = 0; i < N; i++)
    l[i] = f[i];
}

/* { dg-final { scan-assembler "test_vec_unpacks_float_hi_lo_v8si:.*\txvpermi\\.d.*-test_vec_unpacks_float_hi_lo_v8si" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_float_hi_lo_v8si:.*\tvext2xv\\.d\\.w.*-test_vec_unpacks_float_hi_lo_v8si" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_float_hi_lo_v8si:.*\txvffint\\.d\\.l.*-test_vec_unpacks_float_hi_lo_v8si" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_float_hi_lo_v8si:.*\txvffinth\\.d\\.w.*-test_vec_unpacks_float_hi_lo_v8si" } } */
void
test_vec_unpacks_float_hi_lo_v8si (void)
{
  for (int i = 0; i < N; i++)
    d[i] = s[i];
}

/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v8si:.*\tvext2xv\\.d\\.w.*-test_vec_unpacks_hi_lo_v8si" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v8si:.*\txvpermi\\.q.*-test_vec_unpacks_hi_lo_v8si" } } */
void
test_vec_unpacks_hi_lo_v8si (void)
{
  for (int i = 0; i < N; i++)
    l[i] = s[i];
}

/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v16hi:.*\tvext2xv\\.w\\.h.*-test_vec_unpacks_hi_lo_v16hi" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v16hi:.*\txvpermi\\.q.*-test_vec_unpacks_hi_lo_v16hi" } } */
void
test_vec_unpacks_hi_lo_v16hi (void)
{
  for (int i = 0; i < N; i++)
    s[i] = h[i];
}

/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v32qi:.*\tvext2xv\\.h\\.b.*-test_vec_unpacks_hi_lo_v32qi" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v32qi:.*\txvpermi\\.q.*-test_vec_unpacks_hi_lo_v32qi" } } */
void
test_vec_unpacks_hi_lo_v32qi (void)
{
  for (int i = 0; i < N; i++)
    h[i] = c[i];
}

/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v8sf:.*\txvfcvtl\\.d\\.s.*-test_vec_unpacks_hi_lo_v8sf" } } */
/* { dg-final { scan-assembler "test_vec_unpacks_hi_lo_v8sf:.*\txvpermi\\.d.*-test_vec_unpacks_hi_lo_v8sf" } } */
void
test_vec_unpacks_hi_lo_v8sf (void)
{
  for (int i = 0; i < N; i++)
    d[i] = f[i];
}

/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v8si:.*\tvext2xv\\.du\\.wu.*-test_vec_unpacku_hi_lo_v8si" } } */
/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v8si:.*\txvpermi\\.q.*-test_vec_unpacku_hi_lo_v8si" } } */
void
test_vec_unpacku_hi_lo_v8si (void)
{
  for (int i = 0; i < N; i++)
    ul[i] = us[i];
}

/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v16hi:.*\tvext2xv\\.wu\\.hu.*-test_vec_unpacku_hi_lo_v16hi" } } */
/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v16hi:.*\txvpermi\\.q.*-test_vec_unpacku_hi_lo_v16hi" } } */
void
test_vec_unpacku_hi_lo_v16hi (void)
{
  for (int i = 0; i < N; i++)
    us[i] = uh[i];
}

/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v32qi:.*\tvext2xv\\.hu\\.bu.*-test_vec_unpacku_hi_lo_v32qi" } } */
/* { dg-final { scan-assembler "test_vec_unpacku_hi_lo_v32qi:.*\txvpermi\\.q.*-test_vec_unpacku_hi_lo_v32qi" } } */
void
test_vec_unpacku_hi_lo_v32qi (void)
{
  for (int i = 0; i < N; i++)
    uh[i] = uc[i];
}
