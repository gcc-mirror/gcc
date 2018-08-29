/* PR tree-optimization/85259 - Missing -Wstringop-overflow= since r256683
   { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow -ftrack-macro-expansion=0" } */

#define bos1(p) __builtin_object_size (p, 1)
#define strcat(d, s) __builtin___strcat_chk (d, s, bos1 (d))
#define strcpy(d, s) __builtin___strcpy_chk (d, s, bos1 (d))

char a1[1];
char a2[2];
char a3[3];
char a4[4];
char a5[5];
char a6[6];
char a7[7];
char a8[8];

/* Verify that at least one instance of -Wstringop-overflow is issued
   for each pair of strcpy/strcat calls.  */

void test_strcpy_strcat_1 (void)
{
  strcpy (a1, "1"), strcat (a1, "2");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

void test_strcpy_strcat_2 (void)
{
  strcpy (a2, "12"), strcat (a2, "3");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

void test_strcpy_strcat_3 (void)
{
  strcpy (a3, "123"), strcat (a3, "4");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

void test_strcpy_strcat_4 (void)
{
  strcpy (a4, "1234"), strcat (a4, "5");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

void test_strcpy_strcat_5 (void)
{
  strcpy (a5, "12345"), strcat (a5, "6");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

void test_strcpy_strcat_6 (void)
{
  strcpy (a6, "123456"), strcat (a6, "7");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

void test_strcpy_strcat_7 (void)
{
  strcpy (a7, "1234567"), strcat (a7, "8");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

void test_strcpy_strcat_8 (void)
{
  strcpy (a8, "12345678"), strcat (a8, "9");   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}
