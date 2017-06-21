/* This failed -fcompare-debug because register pressure computation
   took debug insns into account.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fsched-pressure -fschedule-insns -fcompare-debug" { target scheduling } } */
/* { dg-options "-O1 -fcompare-debug" { target { ! scheduling } } } */
/* { dg-require-effective-target int32plus } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

int lzo_adler32(int adler, char *buf)
{
  int s1 = adler;
  int s2 = adler;
  s1 += buf[0];
  s2 += s1;
  s1 += buf[1];
  s2 += s1;
  s1 += buf[2];
  s2 += s1;
  s1 += buf[3];
  s2 += s1;
  s1 += buf[4];
  s2 += s1;
  s1 += buf[5];
  s2 += s1;
  s1 += buf[6];
  s2 += s1;
  s1 += buf[7];
  s2 += s1;
  return (s2 << 16) + s1;
}
