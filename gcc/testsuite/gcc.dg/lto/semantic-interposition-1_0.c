/* { dg-lto-do link }  */
/* { dg-require-effective-target shared } */
/* { dg-extra-ld-options { -shared } } */
/* { dg-lto-options {{-O2 -flto -fpic -fdump-ipa-inline-details --shared}} }  */ 
extern int ret1();

int
test()
{
  return ret1();
}
/* { dg-final { scan-wpa-ipa-dump "Inlined 1 calls"  "inline"  } } */

