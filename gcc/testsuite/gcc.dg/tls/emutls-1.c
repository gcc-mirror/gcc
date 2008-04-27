/* { dg-do run { target *-wrs-vxworks } } */
/* { dg-require-effective-target tls } */

/* vxworks' TLS model requires no extra padding on the tls proxy
   objects.  */

__thread int i;
__thread int j;

extern int __tls__i;
extern int __tls__j;

int main ()
{
  int delta = ((char *)&__tls__j - (char *)&__tls__i);

  if (delta < 0)
    delta = -delta;
  
  return delta != 12;
}
