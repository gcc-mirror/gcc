/* Test for bug in conversions from 5-byte UTF-8 sequences in
   cpplib.  */
/* { dg-do run { target { 4byte_wchar_t } } } */
/* { dg-options "-std=gnu99" } */

extern void abort (void);
extern void exit (int);

__WCHAR_TYPE__ ws[] = L"û¿¿¿¿";

int
main (void)
{
  if (ws[0] != L'\U03FFFFFF' || ws[1] != 0)
    abort ();
  exit (0);
}
