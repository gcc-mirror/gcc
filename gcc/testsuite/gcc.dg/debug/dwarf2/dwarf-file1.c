/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "File Entry:|.file" } } */
/* Verify that a file entry is output for this file.  Only systems that
   do not define HAVE_AS_DWARF2_DEBUG_LINE will put out "File Entry:",
   but systems that do define HAVE_AS_DWARF2_DEBUG_LINE will put out a
   ".file" so we check for either.  */

int bar = 3;
