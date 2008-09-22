/* { dg-do compile } */
/* Currently ICEs for IA64, HPPA, MIPS, CRIS, Xtensa, PowerPC, SH and SPARC; see PR33642.  */
/* { dg-xfail-if "PR33642" { hppa*-*-* mips*-*-* powerpc*-*-* cris-*-* crisv32-*-* ia64-*-* xtensa*-*-* sh*-*-* sparc*-*-* } { "*" } { "" } } */
/* Currently ICEs for (x86 && ilp32 && pic).  */
/* { dg-xfail-if "PR33642/36240" { { i?86-*-* x86_64-*-* } && { ilp32 && { ! nonpic } } } { "*" } { "" } } */
/* { dg-prune-output ".*internal compiler error.*" }
/* { dg-options "-frtl-abstract-sequences" } */

char *progName;
int bar0 (char *, ...);
void bar1 (char *);
void exit (int);


#define SAME \
 bar0 ("%s: Bad flag `%s'\n", argv[i], argv[i] );\
 bar1 ( progName ); \
 exit ( 1 );


int foo ( int argc, char *argv[] )
{
    int i;
    for (i = 0; i < argc; i++) {
	switch (argv[i][0]) {
	case 'c':
	    break;
	default: 
	
	    SAME
	    
	    break;
	}
    }
    for (i = 0; i < argc; i++) {
    
	SAME
	
    }
    return 0;
}
