/* { dg-do compile } */
/* Currently ICEs for MIPS and PowerPC; see PR33642.  */
/* { dg-xfail-if "PR33642" { mips*-*-* powerpc*-*-linux* } { "*" } { "" } } */
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
