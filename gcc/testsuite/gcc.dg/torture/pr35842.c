/* { dg-do compile { target { *-*-mingw* } } } */
extern __attribute__((dllimport)) const int SSL_ImplementedCiphers[];
extern void SSL_GetCipherSuiteInfo(int cipherSuite);
void nsCipherInfo(int SSL_NumImplementedCiphers)
{
  int i;
  for (i = 0; i < SSL_NumImplementedCiphers; ++i)
    {
      const int i_id = SSL_ImplementedCiphers[i];
      SSL_GetCipherSuiteInfo(i_id);
    }
}
