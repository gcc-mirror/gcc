/* { dg-do compile } */
/* { dg-require-effective-target avr_tiny } */

typedef struct
{
  char a, b, c;
} abc_t;

extern char varA __attribute__((absdata));
extern char varB __attribute__((absdata));

extern int arrayA[] __attribute__((absdata));
extern int arrayB[] __attribute__((absdata));
extern char arrayC[] __attribute__((address(0x80)));

extern abc_t abc __attribute__((absdata));

char get_1 (void)
{
  return varA;
}

int get_2 (void)
{
  return arrayA[3];
}

char get_3 (void)
{
  return abc.a + abc.b + abc.c;
}


void put_1 (char b)
{
  varB = b;
}

void put_2 (int b)
{
  arrayB[3] = b;
}

void put_3 (void)
{
  abc.a = abc.b = abc.c = 0;
}

void put_4 (void)
{
  arrayC[0] = arrayC[1] = arrayC[2] = 0;
}

/* { dg-final { scan-assembler "lds r\[0-9\]+,varA" } } */
/* { dg-final { scan-assembler "lds r\[0-9\]+,arrayA\\+6" } } */
/* { dg-final { scan-assembler "lds r\[0-9\]+,arrayA\\+6\\+1" } } */
/* { dg-final { scan-assembler "lds r\[0-9\]+,abc" } } */
/* { dg-final { scan-assembler "lds r\[0-9\]+,abc\\+1" } } */
/* { dg-final { scan-assembler "lds r\[0-9\]+,abc\\+2" } } */

/* { dg-final { scan-assembler "sts varB," } } */
/* { dg-final { scan-assembler "sts arrayB\\+6," } } */
/* { dg-final { scan-assembler "sts arrayB\\+6\\+1," } } */
/* { dg-final { scan-assembler "sts arrayC," } } */
/* { dg-final { scan-assembler "sts arrayC\\+1," } } */
/* { dg-final { scan-assembler "sts arrayC\\+2," } } */

/* { dg-final { scan-assembler "sts abc," } } */
/* { dg-final { scan-assembler "sts abc\\+1," } } */
/* { dg-final { scan-assembler "sts abc\\+2," } } */
