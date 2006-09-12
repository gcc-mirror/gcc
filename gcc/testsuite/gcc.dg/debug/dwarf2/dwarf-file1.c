/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "File Entry:" } } */
/* Verify that a file entry is output for this file.  */
int bar = 3;
