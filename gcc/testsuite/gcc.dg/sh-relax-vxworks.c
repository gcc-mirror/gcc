/* Check that -mrelax produces the correct error message.  */
/* { dg-do compile { target { sh-*-vxworks* && nonpic } } } */
/* { dg-error "-mrelax is only supported for RTP PIC" "" { target *-*-* } 0 } */
/* { dg-options "-O1 -mrelax" } */
int x;
