/*
 Contributed by Dodji Seketeli <dodji@redhat.com>
 { dg-options "-gdwarf -dA -fno-merge-debug-strings" }
 { dg-do compile }
 { dg-final { scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_enumeration_type" 1 } }
 { dg-final { scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_enumerator" 2 } }
 { dg-final { scan-assembler-times "ascii \"a.0\"\[\t \]+\[^\n\]*DW_AT_name" 1 { xfail { powerpc-ibm-aix* } } } }
 { dg-final { scan-assembler-times "ascii \"b.0\"\[\t \]+\[^\n\]*DW_AT_name" 1 { xfail { powerpc-ibm-aix* } } } }
 */

enum { a, b };

int v = a;
char s[b];
