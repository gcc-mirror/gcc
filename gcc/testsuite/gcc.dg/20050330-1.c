/* This test is a reduced test case for a bug that caused 
   ICE while bootstrapping with -fmodulo-sched on powerpc-apple-darwin
   related to (PR middle-end/20177).  */
 
/* { dg-do compile } */
/* { dg-options "-O2 -fmodulo-sched" } */

void  
foo ( const char *bytes, int len , char *buf)
{
  int i;
  for ( i = 0; i < len; ++i )
    buf[i] = bytes[i];
}
