/* Everything you wanted to know about your machine and C compiler,
   but didn't know who to ask.  */

#ifndef VERSION
#define VERSION "4.3"
#endif

/* Author: Steven Pemberton, CWI, Amsterdam; steven@cwi.nl
   Bugfixes and upgrades gratefully received.

   Copyright (c) 1988, 1989, 1990 Steven Pemberton, CWI, Amsterdam.
   All rights reserved.

   Changes by Richard Stallman:
   Undef CHAR_BIT, etc., if defined in stdio.h, Richard Stallman, Aug 90.
   In EPROP, avoid a <= old if bad is set, Richard Stallman, May 91.
   Use gstddef.h, not stddef.h, Richard Stallman, Nov 91.
   Don't declare malloc, instead cast the value, Richard Stallman, Nov 91.
   Include sys/types.h before signal.h, Apr 92.
   Support NO_LONG_DOUBLE_IO in f_define and f_rep; new fn fake_f_rep, Apr 92.
   Enclose -f output in #ifndef _FLOAT_H___, Richard Stallman, May 92.

   Change by Jim Wilson:
   Add #undef before every #define, Dec 92.
   Use stddef.h not gstddef.h, Mar 94.

   Changes by Paul Eggert, installed Feb 93:
   (fake_f_rep): Clear all of u, initially.  Make the ints in u unsigned.
   (f_define): Use ordinary constants for long double
   if it's same width as double.  Make __convert_long_double_i unsigned.
   Richard Stallman, May 93:
   In F_check, check NO_LONG_DOUBLE_IO.

   Changes by Stephen Moshier, installed Sep 93:
   (FPROP): Recognize 80387 or 68881 XFmode format.


   COMPILING
   With luck and a following wind, just the following will work:
	cc enquire.c -o enquire
   You may get some messages about unreachable code, which you can ignore.

   If your compiler doesn't support:		add flag:
	signed char (eg pcc)			-DNO_SC
	unsigned char				-DNO_UC
	unsigned short and long			-DNO_UI
	void					-DNO_VOID
	signal(), or setjmp/longjmp()		-DNO_SIG
	%Lf in printf				-DNO_LONG_DOUBLE_IO

   Try to compile first with no flags, and see if you get any errors -
   you might be surprised. (Most non-ANSI compilers need -DNO_SC, though.)
   Some compilers need a -f flag for floating point.

   Don't use any optimisation flags: the program may not work if you do.
   Though "while (a+1.0-a-1.0 == 0.0)" may look like "while(1)" to an
   optimiser, to a floating-point unit there's a world of difference.

   Some compilers offer various flags for different floating point
   modes; it's worth trying all possible combinations of these.

   Add -DID=\"name\" if you want the machine/flags identified in the output.

   FAULTY COMPILERS
   Because of bugs and/or inadequacies, some compilers need the following
   defines:

   If your C preprocessor doesn't have the predefined __FILE__ macro, and
   you don't want to call this file enquire.c but, say, tell.c, add the
   flag -DFILENAME=\"tell.c\" .

   Some compilers won't accept the line "#include FILENAME".
   Add flag -DNO_FILE. In that case, this file *must* be called enquire.c.

   Some compilers can't cope with "#ifdef __FILE__". Use -DFILENAME=
   or -DNO_FILE as above.

   Some naughty compilers define __STDC__, but don't really support it.
   Some define it as 0, in which case we treat it as undefined.
   But if your compiler defines it, and isn't really ANSI C,
   add flag -DNO_STDC. (To those compiler writers: for shame).

   Some naughty compilers define __STDC__, but don't have the stddef.h
   include file. Add flag -DNO_STDDEF.

   Summary of naughty-compiler flags:
   If your compiler doesn't support:		 add flag:
	__FILE__ (and you changed the filename)	-DFILENAME=\"name.c\"
	#ifdef __FILE__				-DNO_FILE or -DFILENAME=...
	#include FILENAME			-DNO_FILE
	__STDC__ (properly)			-DNO_STDC
	stddef.h				-DNO_STDDEF

   Some systems crash when you try to malloc all store. To save users of
   such defective systems too much grief, they may compile with -DNO_MEM,
   which ignores that bit of the code.

   While it is not our policy to support defective compilers, pity has been
   taken on people with compilers that can't produce object files bigger than
   32k (especially since it was an easy addition). Compile the program
   into separate parts like this:
       cc -DSEP -DPASS0 -o p0.o <other flags> enquire.c
       cc -DSEP -DPASS1 -o p1.o <other flags> enquire.c
       cc -DSEP -DPASS2 -o p2.o <other flags> enquire.c
       cc -DSEP -DPASS3 -o p3.o <other flags> enquire.c
       cc -o enquire p0.o p1.o p2.o p3.o

   SYSTEM DEPENDENCIES
   You may possibly need to add some calls to signal() for other sorts of
   exception on your machine than SIGFPE, and SIGOVER. See lines beginning
   #ifdef SIGxxx in main() (and communicate the differences to me!).

   OUTPUT
   Run without argument to get the information as English text. If run
   with argument -l (e.g. enquire -l), output is a series of #define's for
   the ANSI standard limits.h include file, excluding MB_MAX_CHAR. If run
   with argument -f, output is a series of #define's for the ANSI standard
   float.h include file (according to ANSI C Draft of Dec 7, 1988).
   Flag -v gives verbose output: output includes the English text above
   as C comments. The program exit(0)'s if everything went ok, otherwise
   it exits with a positive number, telling how many problems there were.

   VERIFYING THE COMPILER
   If, having produced the float.h and limits.h header files, you want to
   verify that the compiler reads them back correctly (there are a lot of
   boundary cases, of course, like minimum and maximum numbers), you can
   recompile enquire.c with -DVERIFY set (plus the other flags that you used
   when compiling the version that produced the header files). This then
   recompiles the program so that it #includes "limits.h" and "float.h",
   and checks that the constants it finds there are the same as the
   constants it produces. Run the resulting program with enquire -fl.
   Very few compilers have passed without error.
   NB: You *must* recompile with the same compiler and flags, otherwise
   you may get odd results.

   You can also use this option if your compiler already has both files,
   and you want to confirm that this program produces the right results.

   TROUBLESHOOTING.
   This program is now quite trustworthy, and suspicious and wrong output
   may well be caused by bugs in the compiler, not in the program (however
   of course, this is not guaranteed, and no responsibility can be
   accepted, etc.)

   The program only works if overflows are ignored by the C system or
   are catchable with signal().

   If the program fails to run to completion (often with the error message
   "Unexpected signal at point x"), this often turns out to be a bug in the
   C compiler's run-time system. Check what was about to be printed, and
   try to narrow the problem down.

   Another possible problem is that you have compiled the program to produce
   loss-of-precision arithmetic traps. The program cannot cope with these,
   and you should re-compile without them. (They should never be the default).

   Make sure you compiled with optimisation turned off.

   Output preceded by *** WARNING: identifies behaviour of the C system
   deemed incorrect by the program. Likely problems are that printf or
   scanf don't cope properly with certain boundary numbers: this program
   goes to a lot of trouble to calculate its values, and these values
   are mostly boundary numbers. Experience has shown that often printf
   cannot cope with these values, and so in an attempt to increase
   confidence in the output, for each float and double that is printed,
   the printed value is checked by using sscanf to read it back.
       Care is taken that numbers are printed with enough digits to uniquely
   identify them, and therefore that they can be read back identically.
   If the number read back is different, then there is probably a bug in
   printf or sscanf, and the program prints the warning message.
   If the two numbers in the warning look identical, then printf is more
   than likely rounding the last digit(s) incorrectly. To put you at ease
   that the two really are different, the bit patterns of the two numbers
   are also printed. The difference is very likely in the last bit.
       Many scanf's read the minimum double back as 0.0, and similarly cause
   overflow when reading the maximum double. This program quite ruthlessly
   declares all these behaviours faulty. The point is that if you get
   one of these warnings, the output may be wrong, so you should check
   the result carefully if you intend to use the results. Of course, printf
   and sscanf may both be wrong, and cancel each other out, so you should
   check the output carefully anyway.

   The warning that "a cast didn't work" refers to cases like this:

      float f;
      #define C 1.234567890123456789
      f= C;
      if (f != (float) C) printf ("Wrong!");

   A faulty compiler will widen f to double and ignore the cast to float,
   and because there is more accuracy in a double than a float, fail to
   recognise that they are the same. In the actual case in point, f and C
   are passed as parameters to a function that discovers they are not equal,
   so it's just possible that the error was in the parameter passing,
   not in the cast (see function Validate()).
   For ANSI C, which has float constants, the error message is "constant has
   wrong precision".

   REPORTING PROBLEMS
   If the program doesn't work for you for any reason that can't be
   narrowed down to a problem in the C compiler, or it has to be changed in
   order to get it to compile, or it produces suspicious output (like a very
   low maximum float, for instance), please mail the problem and an example
   of the incorrect output to steven@cwi.nl or ..!hp4nl!cwi.nl!steven, so that
   improvements can be worked into future versions; cwi.nl is the European
   backbone, and is connected to uunet and other fine hosts.

   The program tries to catch and diagnose bugs in the compiler/run-time
   system. I would be especially pleased to have reports of failures so
   that I can improve this service.

   I apologise unreservedly for the contorted use of the preprocessor...

   THE SMALL PRINT
   You may copy and distribute verbatim copies of this source file.

   You may modify this source file, and copy and distribute such
   modified versions, provided that you leave the copyright notice
   at the top of the file and also cause the modified file to carry
   prominent notices stating that you changed the files and the date
   of any change; and cause the whole of any work that you distribute
   or publish, that in whole or in part contains or is a derivative of
   this program or any part thereof, to be licensed at no charge to
   all third parties on terms identical to those here.

   If you do have a fix to any problem, please send it to me, so that
   other people can have the benefits.

   While every effort has been taken to make this program as reliable as
   possible, no responsibility can be taken for the correctness of the
   output, nor suitability for any particular use.

   This program is an offshoot of a project funded by public funds.
   If you use this program for research or commercial use (i.e. more
   than just for the fun of knowing about your compiler) mailing a short
   note of acknowledgement may help keep enquire.c supported.

   ACKNOWLEDGEMENTS
   Many people have given time and ideas to making this program what it is.
   To all of them thanks, and apologies for not mentioning them by name.

   HISTORY
   Originally started as a program to generate configuration constants
   for a large piece of software we were writing, which later took on
   a life of its own...
   1.0 Length 6658!; end 1984?
       Unix only. Only printed a dozen maximum int/double values.
   2.0 Length 10535; Spring 1985
       Prints values as #defines (about 20 of them)
       More extensive floating point, using Cody and Waite
       Handles signals better
       Programs around optimisations
       Handles Cybers
   3.0 Length 12648; Aug 1987; prints about 42 values
       Added PASS stuff, so treats float as well as double
   4.0 Length 33891; Feb 1989; prints around 85 values
       First GNU version (for gcc, where they call it hard-params.c)
       Generates float.h and limits.h files
       Handles long double
       Generates warnings for dubious output
   4.1 Length 47738; April 1989
       Added VERIFY and TEST
   4.2 Length 63442; Feb 1990
       Added SEP
       Fixed eps/epsneg
       Added check for pseudo-unsigned chars
       Added description for each #define output
       Added check for absence of defines during verify
       Added prototypes
       Added NO_STDC and NO_FILE
       Fixed alignments output
   4.3 Length 75000; Oct 1990; around 114 lines of output
       Function xmalloc defined, Richard Stallman, June 89.
       Alignments computed from member offsets rather than structure sizes,
          Richard Stallman, Oct 89.
       Print whether char* and int* pointers have the same format;
          also char * and function *.
       Update to Draft C version Dec 7, 1988
	  - types of constants produced in limits.h
	    (whether to put a U after unsigned shorts and chars and
	     whether to output -1024 as (-1023-1))
	  - values of SCHAR_MIN/MAX
	  - values of *_EPSILON (not the smallest but the effective smallest)
       Added FILENAME, since standard C doesn't allow #define __FILE__
       Renamed from config.c to enquire.c
       Added size_t and ptrdiff_t enquiries
       Added promotion enquiries
       Added type checks of #defines
       Added NO_STDDEF
       Changed endian to allow for cases where not all bits are used
       Sanity check for max integrals
       Fixed definition of setjmp for -DNO_SIG
       Moved #define ... 0.0L inside #ifdef STDC, in case some cpp's tokenize
       Added NO_MEM
*/

/* Set FILENAME to the name of this file */
#ifndef FILENAME
#ifdef NO_FILE
#define FILENAME "enquire.c"
#else
#ifdef __FILE__ /* It's a compiler bug if this fails. Compile with -DNO_FILE */
#define FILENAME __FILE__
#else
#define FILENAME "enquire.c"
#endif /* __FILE__ */
#endif /* NO_FILE */
#endif /* FILENAME */

/* If PASS isn't defined, then this is the first pass over this file.  */
#ifndef PASS
#ifndef SEP
#define PASS 1
#define PASS0 1
#define PASS1 1
#endif /* SEP */

/* A description of the ANSI constants */
#define D_CHAR_BIT "Number of bits in a storage unit"
#define D_CHAR_MAX "Maximum char"
#define D_CHAR_MIN "Minimum char"
#define D_SCHAR_MAX "Maximum signed char"
#define D_SCHAR_MIN "Minimum signed char"
#define D_UCHAR_MAX "Maximum unsigned char (minimum is always 0)"

#define D_INT_MAX "Maximum %s"
#define D_INT_MIN "Minimum %s"
#define D_UINT_MAX "Maximum unsigned %s (minimum is always 0)"

#define D_FLT_ROUNDS "Addition rounds to 0: zero, 1: nearest, 2: +inf, 3: -inf, -1: unknown"
#define D_FLT_RADIX "Radix of exponent representation"
#define D_MANT_DIG "Number of base-FLT_RADIX digits in the significand of a %s"
#define D_DIG "Number of decimal digits of precision in a %s"
#define D_MIN_EXP "Minimum int x such that FLT_RADIX**(x-1) is a normalised %s"
#define D_MIN_10_EXP "Minimum int x such that 10**x is a normalised %s"
#define D_MAX_EXP "Maximum int x such that FLT_RADIX**(x-1) is a representable %s"
#define D_MAX_10_EXP "Maximum int x such that 10**x is a representable %s"
#define D_MAX "Maximum %s"
#define D_EPSILON "Difference between 1.0 and the minimum %s greater than 1.0"
#define D_MIN "Minimum normalised %s"

/* Procedure just marks the functions that don't return a result */
#ifdef NO_VOID
#define Procedure int
#else
#define Procedure void
#endif

/* Some bad compilers define __STDC__, when they don't support it.
   Compile with -DNO_STDC to get round this.
*/
#ifndef NO_STDC
#ifdef __STDC__
#if __STDC__ /* If __STDC__ is 0, assume it isn't supported */
#define STDC
#endif
#endif
#endif

/* Stuff different for ANSI C, and old C:
   ARGS and NOARGS are used for function prototypes.
   Volatile is used to reduce the chance of optimisation,
      and to prevent variables being put in registers (when setjmp/longjmp
      wouldn't work as we want)
   Long_double is the longest floating point type available.
   stdc is used in tests like "if (stdc)", which is less ugly than #ifdef.
   U is output after unsigned constants.
 */
#ifdef STDC

#define ARGS(x) x
#define NOARGS (void)
#define Volatile volatile
#define Long_double long double
#define stdc 1
#define U "U"

#else /* Old style C */

#define ARGS(x) ()
#define NOARGS ()
#define Volatile static
#define Long_double double
#define stdc 0
#define U ""

#endif /* STDC */

/* include files */
/* Stdio.h might include limits.h, and limits.h might include float.h, and
   float.h is probably the float.h put together by the gcc makefile to
   cause errors.  We use our special define to assure float.h that we don't
   really need it.  */
#define __GCC_FLOAT_NOT_NEEDED   
#include <stdio.h>

#ifdef STDC
#ifndef NO_STDDEF
#include <stddef.h> /* for size_t: if this fails, define NO_STDDEF */
#endif
#endif

#ifdef NO_SIG
#define jmp_buf int
#else
#include <sys/types.h>
#include <signal.h>
#include <setjmp.h>
#endif

/* Kludge around the possibility that <stdio.h> includes <limits.h> */
#ifdef CHAR_BIT
#undef CHAR_BIT
#undef CHAR_MAX
#undef CHAR_MIN
#undef SCHAR_MAX
#undef SCHAR_MIN
#undef UCHAR_MAX
#undef UCHAR_MIN
#endif

#ifdef VERIFY
#include "limits.h"
#endif

#ifndef SYS_FLOAT_H_WRAP
#define SYS_FLOAT_H_WRAP 0
#endif

#if SYS_FLOAT_H_WRAP || defined VERIFY
#include "float.h"
#endif

#define Vprintf if (V) printf
#define Unexpected(place) if (setjmp(lab)!=0) croak(place)
#define fabs(x) (((x)<0.0)?(-x):(x))

#endif /* PASS */

#ifdef PASS0

/* Prototypes for what's to come: */

int false NOARGS;

#ifdef NO_STDDEF
char *malloc (); /* Old style prototype */
#else
char *malloc ARGS((size_t size));
#endif

Procedure exit ARGS((int status));

char *f_rep ARGS((int precision, Long_double val));
char *fake_f_rep ARGS((char *type, Long_double val));

int maximum_int NOARGS;
int cprop NOARGS;
int basic NOARGS;
Procedure sprop NOARGS;
Procedure iprop NOARGS;
Procedure lprop NOARGS;
Procedure usprop NOARGS;
Procedure uiprop NOARGS;
Procedure ulprop NOARGS;
int fprop ARGS((int bits_per_byte));
int dprop ARGS((int bits_per_byte));
int ldprop ARGS((int bits_per_byte));
Procedure efprop ARGS((int fprec, int dprec, int lprec));
Procedure edprop ARGS((int fprec, int dprec, int lprec));
Procedure eldprop ARGS((int fprec, int dprec, int lprec));

int setmode ARGS((char *s));
Procedure farewell ARGS((int bugs));
Procedure describe ARGS((char *description, char *extra));
Procedure missing ARGS((char *s));
Procedure fmissing ARGS((char *s));
Procedure check_defines NOARGS;
Procedure bitpattern ARGS((char *p, unsigned int size));
int ceil_log ARGS((int base, Long_double x));
Procedure croak ARGS((int place));
Procedure eek_a_bug ARGS((char *problem));
Procedure endian ARGS((int bits_per_byte));
int exponent ARGS((Long_double x, double *fract, int *exp));
int floor_log ARGS((int base, Long_double x));
Procedure f_define ARGS((char *desc, char *extra, char *sort, char *name,
			 int prec, Long_double val, Long_double req,
			 char *mark));
Procedure i_define ARGS((char *desc, char *extra, char *sort, char *name,
			 long val, long lim, long req, char *mark));
Procedure u_define ARGS((char *desc, char *extra, char *sort, char *name,
			 unsigned long val, unsigned long req, char *mark));

#ifdef NO_SIG  /* There's no signal(), or setjmp/longjmp() */

	/* Dummy routines instead */

	int setjmp ARGS((int lab));

	int lab=1;
	int setjmp(lab) int lab; { return(0); }
	Procedure signal(i, p) int i, (*p)(); {}

#else
	jmp_buf lab;
	Procedure overflow(sig) int sig; { /* what to do on over/underflow */
		signal(sig, overflow);
		longjmp(lab, 1);
	}

#endif /*NO_SIG*/

int V= 0,	/* verbose */
    L= 0,	/* produce limits.h */
    F= 0,	/* produce float.h  */
    bugs=0;	/* The number of (possible) bugs in the output */

char co[4], oc[4]; /* Comment starter and ender symbols */

int bits_per_byte; /* the number of bits per unit returned by sizeof() */
int flt_rounds;    /* The calculated value of FLT_ROUNDS */
int flt_radix;     /* The calculated value of FLT_RADIX */

#ifdef TEST
/* Set the fp modes on a SUN with 68881 chip, to check that different
   rounding modes etc. get properly detected.
   Compile with -f68881 for cc, -m68881 for gcc, and with additional flag
   -DTEST. Run with additional parameter +hex-number, to set the 68881 mode
   register to hex-number
*/

/* Bits 0x30 = rounding mode */
#define ROUND_BITS	0x30
#define TO_NEAREST	0x00
#define TO_ZERO		0x10
#define TO_MINUS_INF	0x20
#define TO_PLUS_INF	0x30 /* The SUN FP user's guide seems to be wrong here */

/* Bits 0xc0 = extended rounding */
#define EXT_BITS	0xc0
#define ROUND_EXTENDED	0x00
#define ROUND_SINGLE	0x40
#define ROUND_DOUBLE	0x80

/* Enabled traps */
#define EXE_INEX1  0x100
#define EXE_INEX2  0x200
#define EXE_DZ	   0x400
#define EXE_UNFL   0x800
#define EXE_OVFL  0x1000
#define EXE_OPERR 0x2000
#define EXE_SNAN  0x4000
#define EXE_BSUN  0x8000

/* Only used for testing, on a Sun with 68881 chip */
/* Print the FP mode */
printmode(new) unsigned new; {
	fpmode_(&new);
	printf("New fp mode:\n");
	printf("  Round toward ");
	switch (new & ROUND_BITS) {
	      case TO_NEAREST:   printf("nearest"); break;
	      case TO_ZERO:      printf("zero"); break;
	      case TO_MINUS_INF: printf("minus infinity"); break;
	      case TO_PLUS_INF:  printf("plus infinity"); break;
	      default: printf("???"); break;
	}

	printf("\n  Extended rounding precision: ");

	switch (new & EXT_BITS) {
	      case ROUND_EXTENDED: printf("extended"); break;
	      case ROUND_SINGLE:   printf("single"); break;
	      case ROUND_DOUBLE:   printf("double"); break;
	      default: printf("???"); break;
	}

	printf("\n  Enabled exceptions:");
	if (new & (unsigned) EXE_INEX1) printf(" inex1");
	if (new & (unsigned) EXE_INEX2) printf(" inex2");
	if (new & (unsigned) EXE_DZ)    printf(" dz");
	if (new & (unsigned) EXE_UNFL)  printf(" unfl");
	if (new & (unsigned) EXE_OVFL)  printf(" ovfl");
	if (new & (unsigned) EXE_OPERR) printf(" operr");
	if (new & (unsigned) EXE_SNAN)  printf(" snan");
	if (new & (unsigned) EXE_BSUN)  printf(" bsun");
	printf("\n");
}

/* Only used for testing, on a Sun with 68881 chip */
/* Set the FP mode */
int setmode(s) char *s; {
	unsigned mode=0, dig;
	char c;

	while (*s) {
		c= *s++;
		if  (c>='0' && c<='9') dig= c-'0';
		else if (c>='a' && c<='f') dig= c-'a'+10;
		else if (c>='A' && c<='F') dig= c-'A'+10;
		else return 1;
		mode= mode<<4 | dig;
	}
	printmode(mode);
	return 0;
}
#else
/* ARGSUSED */
int setmode(s) char *s; {
	fprintf(stderr, "Can't set mode: not compiled with TEST\n");
	return(1);
}
#endif

Procedure farewell(bugs) int bugs; {
	if (bugs == 0) exit(0);
	printf("\n%sFor hints on dealing with the ", co);
	if (bugs == 1) printf("problem");
	else printf("%d problems", bugs);
	printf(" above\n   see the section 'TROUBLESHOOTING' in the file ");
	printf("%s%s\n", FILENAME, oc);
	exit(bugs);
}

/* The program has received a signal where it wasn't expecting one */
Procedure croak(place) int place; {
	printf("*** Unexpected signal at point %d\n", place);
	farewell(bugs+1); /* An exit isn't essential here, but avoids loops */
}

/* This is here in case alloca.c is used, which calls this.  */
char *xmalloc(size) unsigned size; {
	char *value = (char *)malloc(size);
	if (value == 0) {
		fprintf(stderr, "Virtual memory exceeded\n");
		exit(bugs+1);
	}
	return value;
}

int maxint;

int maximum_int() {
	/* Find the maximum integer */
	Volatile int newi, int_max, two=2;

	/* Calculate maxint ***********************************/
	/* Calculate 2**n-1 until overflow - then use the previous value  */

	newi=1; int_max=0;

	if (setjmp(lab)==0) { /* Yields int_max */
		while(newi>int_max) {
			int_max=newi;
			newi=newi*two+1;
		}
	}
	Unexpected(0);
	return int_max;
}

int main(argc, argv) int argc; char *argv[]; {
	int dprec, fprec, lprec;
	int i; char *s; int bad;

#ifdef SIGFPE
	signal(SIGFPE, overflow);
#endif
#ifdef SIGOVER
	signal(SIGOVER, overflow);
#endif
/* Add more calls as necessary */

	Unexpected(1);

	bad=0;
	for (i=1; i < argc; i++) {
		s= argv[i];
		if (*s == '-') {
			s++;
			while (*s) {
				switch (*(s++)) {
				      case 'v': V=1; break;
				      case 'l': L=1; break;
				      case 'f': F=1; break;
				      default: bad=1; break;
				}
			}
		} else if (*s == '+') {
			s++;
			bad= setmode(s);
		} else bad= 1;
	}
	if (bad) {
		fprintf(stderr,
			"Usage: %s [-vlf]\n  v=Verbose l=Limits.h f=Float.h\n",
			argv[0]);
		exit(1);
	}
	if (L || F) {
		co[0]= '/'; oc[0]= ' ';
		co[1]= '*'; oc[1]= '*';
		co[2]= ' '; oc[2]= '/';
		co[3]= '\0'; oc[3]= '\0';
	} else {
		co[0]= '\0'; oc[0]= '\0';
		V=1;
	}

	if (L) printf("%slimits.h%s\n", co, oc);
	if (F) printf("%sfloat.h%s\n", co, oc);
	if (F) {
		printf ("#ifndef _FLOAT_H___\n");
		printf ("#define _FLOAT_H___\n");
		if (SYS_FLOAT_H_WRAP)
			printf ("#include_next <float.h>\n");
	}
#ifdef ID
	printf("%sProduced on %s by enquire version %s, CWI, Amsterdam%s\n",
	       co, ID, VERSION, oc);
#else
	printf("%sProduced by enquire version %s, CWI, Amsterdam%s\n",
	       co, VERSION, oc);
#endif

#ifdef VERIFY
	printf("%sVerification phase%s\n", co, oc);
#endif

#ifdef NO_SIG
	Vprintf("%sCompiled without signal(): %s%s\n",
		co,
		"there's nothing that can be done if overflow occurs",
		oc);
#endif
#ifdef NO_SC
	Vprintf("%sCompiled without signed char%s\n", co, oc);
#endif
#ifdef NO_UC
	Vprintf("%Compiled without unsigned char%s\n", co, oc);
#endif
#ifdef NO_UI
	Vprintf("%Compiled without unsigned short or long%s\n", co, oc);
#endif
#ifdef __STDC__
	Vprintf("%sCompiler claims to be ANSI C level %d%s\n",
		co, __STDC__, oc);
#else
	Vprintf("%sCompiler does not claim to be ANSI C%s\n", co, oc);
#endif
	printf("\n");
	check_defines();

	maxint= maximum_int();
	bits_per_byte= basic();
	Vprintf("\n");
	if (F||V) {
		fprec= fprop(bits_per_byte);
		dprec= dprop(bits_per_byte);
		lprec= ldprop(bits_per_byte);
		efprop(fprec, dprec, lprec);
		edprop(fprec, dprec, lprec);
		eldprop(fprec, dprec, lprec);
	}
#ifndef NO_MEM
	if (V) {
		unsigned int size;
		long total;
		/* An extra goody: the approximate amount of data-space */
		/* Allocate store until no more available */
		/* Different implementations have a different argument type
		   to malloc. Here we assume that it's the same type as
		   that which sizeof() returns */
		size=1<<((bits_per_byte*sizeof(int))-2);
		total=0;
		while (size!=0) {
			while ( malloc((false()?sizeof(int):size)) !=
			        (char *)NULL
			       ) {
				total+=(size/2);
			}
			size/=2;
		}

		Vprintf("%sMemory allocable ~= %ld Kbytes%s\n",
			co, (total+511)/512, oc);
	}
#endif
	if (F) {
		printf ("#endif %s _FLOAT_H___%s\n", co, oc);
	}
	farewell(bugs);
	return bugs; /* To keep compilers and lint happy */
}

Procedure eek_a_bug(problem) char *problem; {
	/* The program has discovered a problem */
	printf("\n%s*** WARNING: %s%s\n", co, problem, oc);
	bugs++;
}

Procedure describe(description, extra) char *description, *extra; {
	/* Produce the description for a #define */
	printf("   %s", co);
	printf(description, extra);
	printf("%s\n", oc);
}

Procedure i_define(desc, extra, sort, name, val, lim, req, mark)
     char *desc, *extra, *sort, *name; long val, lim, req; char *mark; {
	if (SYS_FLOAT_H_WRAP && F && val == req)
		return;
	/* Produce a #define for a signed int type */
	describe(desc, extra);
	printf("#undef %s%s\n", sort, name);
	if (val >= 0) {
		printf("#define %s%s %ld%s\n", sort, name, val, mark);
	} else if (val + lim < 0) {
		/* We may not produce a constant like -1024 if the max
		   allowable value is 1023. It has then to be output as
		   -1023-1. lim is the max allowable value.  */
		printf("#define %s%s (%ld%s%ld%s)\n",
		       sort, name, -lim, mark, val+lim, mark);
	} else {
		printf("#define %s%s (%ld%s)\n", sort, name, val, mark);
	}
#ifdef VERIFY
	if (val != req) {
		printf("%s*** Verify failed for above #define!\n", co);
		printf("       Compiler has %ld for value%s\n\n", req, oc);
		bugs++;
	}
#endif
	Vprintf("\n");
}

Procedure u_define(desc, extra, sort, name, val, req, mark)
     char *desc, *extra, *sort, *name; unsigned long val, req; char *mark; {
	/* Produce a #define for an unsigned value */
	describe(desc, extra);
	printf("#undef %s%s\n", sort, name);
	printf("#define %s%s %lu%s%s\n", sort, name, val, U, mark);
#ifdef VERIFY
	if (val != req) {
		printf("%s*** Verify failed for above #define!\n", co);
		printf("       Compiler has %lu for value%s\n\n", req, oc);
		bugs++;
	}
#endif
	Vprintf("\n");
}

Procedure f_define(desc, extra, sort, name, precision, val, req, mark)
     char *desc, *extra, *sort, *name; int precision;
     Long_double val, req; char *mark; {
	if (SYS_FLOAT_H_WRAP && F && val == req)
		return;
	/* Produce a #define for a float/double/long double */
	describe(desc, extra);
	printf ("#undef %s%s\n", sort, name);
	if (stdc) {
#ifdef NO_LONG_DOUBLE_IO
		static int union_defined = 0;
		if (sizeof(double) != sizeof(Long_double)
		    && !strcmp(sort, "LDBL")) {
			if (!union_defined) {
				printf("#ifndef __LDBL_UNION__\n");
				printf("#define __LDBL_UNION__\n");
				printf("union __convert_long_double {\n");
				printf("  unsigned __convert_long_double_i[4];\n");
				printf("  long double __convert_long_double_d;\n");
				printf("};\n");
				printf("#endif\n");
				union_defined = 1;
			}
			printf("#define %s%s %s\n",
			       sort, name, fake_f_rep("long double", val));
		} else {
			printf("#define %s%s %s%s\n",
			       sort, name, f_rep(precision, val), mark);
		}
#else
		printf("#define %s%s %s%s\n",
		       sort, name, f_rep(precision, val), mark);
#endif
	} else if (*mark == 'F') {
		/* non-ANSI C has no float constants, so cast the constant */
		printf("#define %s%s ((float)%s)\n",
		       sort, name, f_rep(precision, val));
	} else {
		printf("#define %s%s %s\n", sort, name, f_rep(precision, val));
	}
	Vprintf("\n");
}

int floor_log(base, x) int base; Long_double x; {
	/* return floor(log base(x)) */
	int r=0;
	while (x>=base) { r++; x/=base; }
	return r;
}

int ceil_log(base, x) int base; Long_double x; {
	int r=0;
	while (x>1.0) { r++; x/=base; }
	return r;
}

int exponent(x, fract, exp) Long_double x; double *fract; int *exp; {
	/* Split x into a fraction and a power of ten;
	   returns 0 if x is unusable, 1 otherwise.
	   Only used for error messages about faulty output.
	*/
	int r=0, neg=0;
	Long_double old;
	*fract=0.0; *exp=0;
	if (x<0.0) {
		x= -x;
		neg= 1;
	}
	if (x==0.0) return 1;
	if (x>=10.0) {
		while (x>=10.0) {
			old=x; r++; x/=10.0;
			if (old==x) return 0;
		}
	} else {
		while (x<1.0) {
			old=x; r--; x*=10.0;
			if (old==x) return 0;
		}
	}
	if (neg) *fract= (double) -x;
	else *fract=(double) x;
	*exp=r;
	return 1;
}

/* Print a value of type TYPE with value VAL,
   assuming that sprintf can't handle this type properly (without truncation).
   We create an expression that uses type casting to create the value from
   a bit pattern.  */

char *fake_f_rep(type, val) char *type; Long_double val; {
	static char buf[1024];
	union { unsigned int i[4]; Long_double ld;} u;
	u.i[0] = u.i[1] = u.i[2] = u.i[3] = 0;
	u.ld = val;
	sprintf(buf, "(__extension__ ((union __convert_long_double) {__convert_long_double_i: {0x%x, 0x%x, 0x%x, 0x%x}}).__convert_long_double_d)",
		u.i[0], u.i[1], u.i[2], u.i[3]);
	return buf;
}

char *f_rep(precision, val) int precision; Long_double val; {
	/* Return the floating representation of val */
	static char buf[1024];
#ifdef NO_LONG_DOUBLE_IO
	if (1)
#else
	if (sizeof(double) == sizeof(Long_double))
#endif
	{
		double d = val;
		/* Assume they're the same, and use non-stdc format */
		/* This is for stdc compilers using non-stdc libraries */
		sprintf(buf, "%.*e", precision, d);
	} else {
		/* It had better support Le then */
		sprintf(buf, "%.*Le", precision, val);
	}
	return buf;
}

Procedure bitpattern(p, size) char *p; unsigned int size; {
	/* Printf the bit-pattern of p */
	char c;
	int i, j;

	for (i=1; i<=size; i++) {
		c= *p;
		p++;
		for (j=bits_per_byte-1; j>=0; j--)
			printf("%c", (c>>j)&1 ? '1' : '0');
		if (i!=size) printf(" ");
	}
}

#define Order(x, px, mode)\
   printf("%s%s ", co, mode); for (i=0; i<sizeof(x); i++) px[i]= ab[i]; \
   for (i=1; i<=sizeof(x); i++) { c=((x>>(bits_per_byte*(sizeof(x)-i)))&mask);\
      putchar(c==0 ? '?' : (char)c); }\
   printf("%s\n", oc);

Procedure endian(bits_per_byte) int bits_per_byte; {
	/* Printf the byte-order used on this machine */
	/*unsigned*/ short s=0;
	/*unsigned*/ int j=0;
	/*unsigned*/ long l=0;

	char *ps= (char *) &s,
	     *pj= (char *) &j,
	     *pl= (char *) &l,
	     *ab= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	unsigned int mask, i, c;

	mask=0;
	for (i=1; i<=(unsigned)bits_per_byte; i++) mask= (mask<<1)|1;

	if (V) {
		printf("%sCHARACTER ORDER%s\n", co, oc);
		Order(s, ps, "short:");
		Order(j, pj, "int:  ");
		Order(l, pl, "long: ");
	}
}

Procedure missing(s) char *s; {
	printf("%s*** #define %s missing from limits.h%s\n", co, s, oc);
	bugs++;
}

Procedure fmissing(s) char *s; {
	printf("%s*** #define %s missing from float.h%s\n", co, s, oc);
	bugs++;
}

/* To try and fool optimisers */
int false() { return 0; }

#define Promoted(x) (false()?(x):(-1))
#define is_signed(x) (Promoted(x) < 0)
#define sign_of(x) ((x)?"signed":"unsigned")
#define Signed 1
#define Unsigned 0
#define sgn(x) ((is_signed(x))?Signed:Unsigned)

#define showtype(t, x) Vprintf("%s%s %s %s%s\n", co, t, sign_of(is_signed(x)), type_of(sizeof(x)), oc)

char *type_of(x) int x; {
	if (x == sizeof(char)) {
		if (sizeof(char) == sizeof(int)) return "char/short/int";
		if (sizeof(char) == sizeof(short)) return "char/short";
		return "char";
	}
	if (x == sizeof(short)) {
		if (sizeof(short) == sizeof(int)) return "short/int";
		return "short";
	}
	if (x == sizeof(int)) {
		if (sizeof(int) == sizeof(long)) return "int/long";
		return "int";
	}
	if (x == sizeof(long)) return "long";
	return "unknown-type";
}

char *ftype_of(x) int x; {
	if (x == sizeof(float)) {
		return "float";
	}
	if (x == sizeof(double)) {
		if (sizeof(double) == sizeof(Long_double))
		  return "(long)double";
		return "double";
	}
	if (x == sizeof(Long_double)) {
		return "long double";
	}
	return "unknown-type";
}

Procedure typerr(name, esign, esize, sign, size)
  char *name; int esign, esize, sign, size;
{
       Vprintf("*** %s has wrong type: expected %s %s, found %s %s\n",
	       name, sign_of(esign), type_of(esize),
	       sign_of(sign), type_of(size));
}

Procedure ftyperr(name, esize, size) char *name; int esize, size; {
       Vprintf("*** %s has wrong type: expected %s, found %s\n",
	       name, ftype_of(esize), ftype_of(size));
}

int promotions() {
	int si = 0; long sl = 0;
	unsigned int ui; unsigned long ul;
	short ss; unsigned short us;

	Vprintf("\n%sPROMOTIONS%s\n", co, oc);

	if (
	    /* Possible warnings here; no problem */
	    (sizeof(Promoted(si)) != sizeof(int)) ||
	    (sizeof(Promoted(sl)) != sizeof(long)) ||
	    (sizeof(Promoted(ss)) != sizeof(int)) ||
	    (sizeof(Promoted(ui)) != sizeof(int)) ||
	    (sizeof(Promoted(ul)) != sizeof(long)) ||
	    (sizeof(Promoted(us)) != sizeof(int)) ||
	    is_signed(ui) || is_signed(ul) ||
	    !is_signed(si) || !is_signed(sl)
	    )
	  {
	    eek_a_bug("promotions don't work properly in conditional expressions\n");
	  }

	showtype("unsigned short promotes to", Promoted((unsigned short) 0));
	showtype("long+unsigned gives", sl+ui);
	return 0;
}

#define checktype(x, n, s, t) if((sgn(x)!=s)||(sizeof(x)!=sizeof(t))) typerr(n, s, sizeof(t), sign_of(x), sizeof(x));

#define fchecktype(x, n, t) if (sizeof(x) != sizeof(t)) ftyperr(n, sizeof(x), sizeof(t));

Procedure check_defines() {
	/* ensure that all #defines are present and have the correct type */
#ifdef VERIFY
	int usign;

#ifdef NO_UI
	usign= Signed;
#else
	/* Implementations promote unsigned short differently */
	usign= is_signed((unsigned short) 0);
#endif

	if (L) {
#ifdef CHAR_BIT
	checktype(CHAR_BIT, "CHAR_BIT", Signed, int);
#else
	missing("CHAR_BIT");
#endif
#ifdef CHAR_MAX
	checktype(CHAR_MAX, "CHAR_MAX", Signed, int);
#else
	missing("CHAR_MAX");
#endif
#ifdef CHAR_MIN
	checktype(CHAR_MIN, "CHAR_MIN", Signed, int);
#else
	missing("CHAR_MIN");
#endif
#ifdef SCHAR_MAX
	checktype(SCHAR_MAX, "SCHAR_MAX", Signed, int);
#else
	missing("SCHAR_MAX");
#endif
#ifdef SCHAR_MIN
	checktype(SCHAR_MIN, "SCHAR_MIN", Signed, int);
#else
	missing("SCHAR_MIN");
#endif
#ifdef UCHAR_MAX
	checktype(UCHAR_MAX, "UCHAR_MAX", Signed, int);
#else
	missing("UCHAR_MAX");
#endif
#ifdef SHRT_MAX
	checktype(SHRT_MAX, "SHRT_MAX", Signed, int);
#else
	missing("SHRT_MAX");
#endif
#ifdef SHRT_MIN
	checktype(SHRT_MIN, "SHRT_MIN", Signed, int);
#else
	missing("SHRT_MIN");
#endif
#ifdef INT_MAX
	checktype(INT_MAX, "INT_MAX", Signed, int);
#else
	missing("INT_MAX");
#endif
#ifdef INT_MIN
	checktype(INT_MIN, "INT_MIN", Signed, int);
#else
	missing("INT_MIN");
#endif
#ifdef LONG_MAX
	checktype(LONG_MAX, "LONG_MAX", Signed, long);
#else
	missing("LONG_MAX");
#endif
#ifdef LONG_MIN
	checktype(LONG_MIN, "LONG_MIN", Signed, long);
#else
	missing("LONG_MIN");
#endif
#ifdef USHRT_MAX
	checktype(USHRT_MAX, "USHRT_MAX", usign, int);
#else
	missing("USHRT_MAX");
#endif
#ifdef UINT_MAX
	checktype(UINT_MAX, "UINT_MAX", Unsigned, int);
#else
	missing("UINT_MAX");
#endif
#ifdef ULONG_MAX
	checktype(ULONG_MAX, "ULONG_MAX", Unsigned, long);
#else
	missing("ULONG_MAX");
#endif
	} /* if (L) */

	if (F) {
#ifdef FLT_RADIX
	checktype(FLT_RADIX, "FLT_RADIX", Signed, int);
#else
	fmissing("FLT_RADIX");
#endif
#ifdef FLT_MANT_DIG
	checktype(FLT_MANT_DIG, "FLT_MANT_DIG", Signed, int);
#else
	fmissing("FLT_MANT_DIG");
#endif
#ifdef FLT_DIG
	checktype(FLT_DIG, "FLT_DIG", Signed, int);
#else
	fmissing("FLT_DIG");
#endif
#ifdef FLT_ROUNDS
	checktype(FLT_ROUNDS, "FLT_ROUNDS", Signed, int);
#else
	fmissing("FLT_ROUNDS");
#endif
#ifdef FLT_EPSILON
	fchecktype(FLT_EPSILON, "FLT_EPSILON", float);
#else
	fmissing("FLT_EPSILON");
#endif
#ifdef FLT_MIN_EXP
	checktype(FLT_MIN_EXP, "FLT_MIN_EXP", Signed, int);
#else
	fmissing("FLT_MIN_EXP");
#endif
#ifdef FLT_MIN
	fchecktype(FLT_MIN, "FLT_MIN", float);
#else
	fmissing("FLT_MIN");
#endif
#ifdef FLT_MIN_10_EXP
	checktype(FLT_MIN_10_EXP, "FLT_MIN_10_EXP", Signed, int);
#else
	fmissing("FLT_MIN_10_EXP");
#endif
#ifdef FLT_MAX_EXP
	checktype(FLT_MAX_EXP, "FLT_MAX_EXP", Signed, int);
#else
	fmissing("FLT_MAX_EXP");
#endif
#ifdef FLT_MAX
	fchecktype(FLT_MAX, "FLT_MAX", float);
#else
	fmissing("FLT_MAX");
#endif
#ifdef FLT_MAX_10_EXP
	checktype(FLT_MAX_10_EXP, "FLT_MAX_10_EXP", Signed, int);
#else
	fmissing("FLT_MAX_10_EXP");
#endif
#ifdef DBL_MANT_DIG
	checktype(DBL_MANT_DIG, "DBL_MANT_DIG", Signed, int);
#else
	fmissing("DBL_MANT_DIG");
#endif
#ifdef DBL_DIG
	checktype(DBL_DIG, "DBL_DIG", Signed, int);
#else
	fmissing("DBL_DIG");
#endif
#ifdef DBL_EPSILON
	fchecktype(DBL_EPSILON, "DBL_EPSILON", double);
#else
	fmissing("DBL_EPSILON");
#endif
#ifdef DBL_MIN_EXP
	checktype(DBL_MIN_EXP, "DBL_MIN_EXP", Signed, int);
#else
	fmissing("DBL_MIN_EXP");
#endif
#ifdef DBL_MIN
	fchecktype(DBL_MIN, "DBL_MIN", double);
#else
	fmissing("DBL_MIN");
#endif
#ifdef DBL_MIN_10_EXP
	checktype(DBL_MIN_10_EXP, "DBL_MIN_10_EXP", Signed, int);
#else
	fmissing("DBL_MIN_10_EXP");
#endif
#ifdef DBL_MAX_EXP
	checktype(DBL_MAX_EXP, "DBL_MAX_EXP", Signed, int);
#else
	fmissing("DBL_MAX_EXP");
#endif
#ifdef DBL_MAX
	fchecktype(DBL_MAX, "DBL_MAX", double);
#else
	fmissing("DBL_MAX");
#endif
#ifdef DBL_MAX_10_EXP
	checktype(DBL_MAX_10_EXP, "DBL_MAX_10_EXP", Signed, int);
#else
	fmissing("DBL_MAX_10_EXP");
#endif
#ifdef STDC
#ifdef LDBL_MANT_DIG
	checktype(LDBL_MANT_DIG, "LDBL_MANT_DIG", Signed, int);
#else
	fmissing("LDBL_MANT_DIG");
#endif
#ifdef LDBL_DIG
	checktype(LDBL_DIG, "LDBL_DIG", Signed, int);
#else
	fmissing("LDBL_DIG");
#endif
#ifdef LDBL_EPSILON
	fchecktype(LDBL_EPSILON, "LDBL_EPSILON", long double);
#else
	fmissing("LDBL_EPSILON");
#endif
#ifdef LDBL_MIN_EXP
	checktype(LDBL_MIN_EXP, "LDBL_MIN_EXP", Signed, int);
#else
	fmissing("LDBL_MIN_EXP");
#endif
#ifdef LDBL_MIN
	fchecktype(LDBL_MIN, "LDBL_MIN", long double);
#else
	fmissing("LDBL_MIN");
#endif
#ifdef LDBL_MIN_10_EXP
	checktype(LDBL_MIN_10_EXP, "LDBL_MIN_10_EXP", Signed, int);
#else
	fmissing("LDBL_MIN_10_EXP");
#endif
#ifdef LDBL_MAX_EXP
	checktype(LDBL_MAX_EXP, "LDBL_MAX_EXP", Signed, int);
#else
	fmissing("LDBL_MAX_EXP");
#endif
#ifdef LDBL_MAX
	fchecktype(LDBL_MAX, "LDBL_MAX", long double);
#else
	fmissing("LDBL_MAX");
#endif
#ifdef LDBL_MAX_10_EXP
	checktype(LDBL_MAX_10_EXP, "LDBL_MAX_10_EXP", Signed, int);
#else
	fmissing("LDBL_MAX_10_EXP");
#endif
#endif /* STDC */
	} /* if (F) */
#endif /* VERIFY */
}

#ifdef VERIFY
#ifndef SCHAR_MAX
#define SCHAR_MAX	char_max
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN	char_min
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX	char_max
#endif
#endif /* VERIFY */

#ifndef CHAR_BIT
#define CHAR_BIT	char_bit
#endif
#ifndef CHAR_MAX
#define CHAR_MAX	char_max
#endif
#ifndef CHAR_MIN
#define CHAR_MIN	char_min
#endif
#ifndef SCHAR_MAX
#define SCHAR_MAX	char_max
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN	char_min
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX	char_max
#endif

int cprop() {
	/* Properties of type char */
	Volatile char c, char_max, char_min;
	Volatile int bits_per_byte, c_signed;
	long char_bit;

	Unexpected(2);

	/* Calculate number of bits per character *************************/
	c=1; bits_per_byte=0;
	do { c=c<<1; bits_per_byte++; } while(c!=0);
	c= (char)(-1);
	if (((int)c)<0) c_signed=1;
	else c_signed=0;
	Vprintf("%schar = %d bits, %ssigned%s\n",
		co, (int)sizeof(c)*bits_per_byte, (c_signed?"":"un"), oc);
	char_bit=(long)(sizeof(c)*bits_per_byte);
	if (L) i_define(D_CHAR_BIT, "", "CHAR", "_BIT",
			char_bit, 0L, (long) CHAR_BIT, "");

	c=0; char_max=0;
	c++;
	if (setjmp(lab)==0) { /* Yields char_max */
		while (c>char_max) {
			char_max=c;
			c++;
		}
	} else {
		Vprintf("%sCharacter overflow generates a trap!%s\n", co, oc);
	}
	c=0; char_min=0;
	c--;
	if (setjmp(lab)==0) { /* Yields char_min */
		while (c<char_min) {
			char_min=c;
			c--;
		}
	}
	if (c_signed && char_min == 0) {
		Vprintf("%sBEWARE! Chars are pseudo-unsigned:%s\n", co, oc);
		Vprintf("%s   %s%s%s\n",
			"They contain only nonnegative values, ",
			"but sign extend when used as integers.", co, oc);
	}
	Unexpected(3);

	if (L) {
		/* Because of the integer promotions, you must use a U after
		   the MAX_CHARS in the following cases */
		if ((sizeof(char) == sizeof(int)) && !c_signed) {
			u_define(D_CHAR_MAX, "", "CHAR", "_MAX",
				 (long) char_max,
				 (long) CHAR_MAX, "");
		} else {
			i_define(D_CHAR_MAX, "", "CHAR", "_MAX",
				 (long) char_max, 0L,
				 (long) CHAR_MAX, "");
		}
		i_define(D_CHAR_MIN, "", "CHAR", "_MIN",
			 (long) char_min, (long) maxint,
			 (long) CHAR_MIN, "");
		if (c_signed) {
			i_define(D_SCHAR_MAX, "", "SCHAR", "_MAX",
				 (long) char_max, 0L,
				 (long) SCHAR_MAX, "");
			i_define(D_SCHAR_MIN, "", "SCHAR", "_MIN",
				 (long) char_min, (long) maxint,
				 (long) SCHAR_MIN, "");
		} else {
			if (sizeof(char) == sizeof(int)) {
				u_define(D_UCHAR_MAX, "", "UCHAR", "_MAX",
					 (long) char_max,
					 (long) UCHAR_MAX, "");
			} else {
				i_define(D_UCHAR_MAX, "", "UCHAR", "_MAX",
					 (long) char_max, 0L,
					 (long) UCHAR_MAX, "");
			}
		}

		if (c_signed) {
#ifndef NO_UC
			Volatile unsigned char c, char_max;
			c=0; char_max=0;
			c++;
			if (setjmp(lab)==0) { /* Yields char_max */
				while (c>char_max) {
					char_max=c;
					c++;
				}
			}
			Unexpected(4);
			if (sizeof(char) == sizeof(int)) {
				u_define(D_UCHAR_MAX, "", "UCHAR", "_MAX",
					 (long) char_max,
					 (long) UCHAR_MAX, "");
			} else {
				i_define(D_UCHAR_MAX, "", "UCHAR", "_MAX",
					 (long) char_max, 0L,
					 (long) UCHAR_MAX, "");
			}
#endif
		} else {
#ifndef NO_SC
/* Define NO_SC if this gives a syntax error */ Volatile signed char c, char_max, char_min;
			c=0; char_max=0;
			c++;
			if (setjmp(lab)==0) { /* Yields char_max */
				while (c>char_max) {
					char_max=c;
					c++;
				}
			}
			c=0; char_min=0;
			c--;
			if (setjmp(lab)==0) { /* Yields char_min */
				while (c<char_min) {
					char_min=c;
					c--;
				}
			}
			Unexpected(5);
			i_define(D_SCHAR_MIN, "", "SCHAR", "_MIN",
				 (long) char_min, (long) maxint,
				 (long) SCHAR_MIN, "");
			i_define(D_SCHAR_MAX, "", "SCHAR", "_MAX",
				 (long) char_max, 0L,
				 (long) SCHAR_MAX, "");
#endif /* NO_SC */
		}
	}
	return bits_per_byte;
}

int basic() {
	/* The properties of the basic types.
	   Returns number of bits per sizeof unit */
	Volatile int bits_per_byte;
	typedef int function ();
	int variable;
	int *p, *q;

	Vprintf("%sSIZES%s\n", co, oc);
	bits_per_byte= cprop();

	/* Shorts, ints and longs *****************************************/
	Vprintf("%sshort=%d int=%d long=%d float=%d double=%d bits %s\n",
		co,
		(int) sizeof(short)*bits_per_byte,
		(int) sizeof(int)*bits_per_byte,
		(int) sizeof(long)*bits_per_byte,
		(int) sizeof(float)*bits_per_byte,
		(int) sizeof(double)*bits_per_byte, oc);
	if (stdc) {
		Vprintf("%slong double=%d bits%s\n",
			co, (int) sizeof(Long_double)*bits_per_byte, oc);
	}
	Vprintf("%schar*=%d bits%s%s\n",
		co, (int)sizeof(char *)*bits_per_byte,
		sizeof(char *)>sizeof(int)?" BEWARE! larger than int!":"",
		oc);
	Vprintf("%sint* =%d bits%s%s\n",
		co, (int)sizeof(int *)*bits_per_byte,
		sizeof(int *)>sizeof(int)?" BEWARE! larger than int!":"",
		oc);
	Vprintf("%sfunc*=%d bits%s%s\n",
		co, (int)sizeof(function *)*bits_per_byte,
		sizeof(function *)>sizeof(int)?" BEWARE! larger than int!":"",
		oc);
if (V) printf ("%s%s %s %s%s\n", co, "Type size_t is",
		       ((((false()?( sizeof(int)):(-1))  < 0) )?
			"signed":"unsigned") ,
		       type_of(sizeof(
				      sizeof(int)+0
				      )
			       ),
	       oc);
	showtype("Type size_t is", sizeof(0));

	/* Alignment constants ********************************************/

#define alignment(TYPE) \
	((long)((char *)&((struct{char c; TYPE d;}*)0)->d - (char *) 0))

	Vprintf("\n%sALIGNMENTS%s\n", co, oc);

	Vprintf("%schar=%ld short=%ld int=%ld long=%ld%s\n",
		co,
		alignment(char), alignment(short),
		alignment(int), alignment(long),
		oc);

	Vprintf("%sfloat=%ld double=%ld%s\n",
		co,
		alignment(float), alignment(double),
		oc);

	if (stdc) {
		Vprintf("%slong double=%ld%s\n",
			co,
			alignment(Long_double),
			oc);
	}
	Vprintf("%schar*=%ld int*=%ld func*=%ld%s\n",
		co,
		alignment(char *), alignment(int *), alignment(function *),
		oc);

	Vprintf("\n");

	/* Ten little endians *********************************************/

	endian(bits_per_byte);

	/* Pointers *******************************************************/

	Vprintf("\n%sPROPERTIES OF POINTERS%s\n", co, oc);

	if ((long) (char *) &variable == (long) (int *) &variable) {
		Vprintf("%sChar and int pointer formats seem identical%s\n",
		       co, oc);
	} else {
		Vprintf("%sChar and int pointer formats are different%s\n",
		       co, oc);
	}
	if ((long) (char *) &variable == (long) (function *) &variable) {
		Vprintf("%sChar and function pointer formats seem identical%s\n",
		       co, oc);
	} else {
		Vprintf("%sChar and function pointer formats are different%s\n",
		       co, oc);
	}

	if (V) {
		if ("abcd"=="abcd")
			printf("%sStrings are shared%s\n", co, oc);
		else printf("%sStrings are not shared%s\n", co, oc);
	}

	p=0; q=0;
	showtype("Type ptrdiff_t is", p-q);

	Vprintf("\n%sPROPERTIES OF INTEGRAL TYPES%s\n", co, oc);

	sprop();
	iprop();
	lprop();
	usprop();
	uiprop();
	ulprop();

	promotions();

	Unexpected(6);

	return bits_per_byte;
}

#else /* not PASS0 */

#ifdef SEP
extern jmp_buf lab;
extern int V, L, F, bugs, bits_per_byte;
extern char co[], oc[];
extern char *f_rep();
#endif /* SEP */
#endif /* ifdef PASS0 */

/* As I said, I apologise for the contortions below. The functions are
   expanded by the preprocessor twice or three times (for float and double,
   and maybe for long double, and for short, int and long). That way,
   I never make a change to one that I forget to make to the other.
   You can look on it as C's fault for not supporting multi-line macro's.
   This whole file is read 3 times by the preprocessor, with PASSn set for
   n=1, 2 or 3, to decide which parts to reprocess.
*/

/* #undef on an already undefined thing is (wrongly) flagged as an error
   by some compilers, therefore the #ifdef that follows:
*/
#ifdef Number
#undef Number
#undef THING
#undef Thing
#undef thing
#undef FPROP
#undef Fname
#undef Store
#undef Sum
#undef Diff
#undef Mul
#undef Div
#undef ZERO
#undef HALF
#undef ONE
#undef TWO
#undef THREE
#undef FOUR
#undef Self
#undef F_check
#undef Validate
#undef EPROP
#undef MARK

/* These are the float.h constants */
#undef F_RADIX
#undef F_MANT_DIG
#undef F_DIG
#undef F_ROUNDS
#undef F_EPSILON
#undef F_MIN_EXP
#undef F_MIN
#undef F_MIN_10_EXP
#undef F_MAX_EXP
#undef F_MAX
#undef F_MAX_10_EXP
#endif

#ifdef Integer
#undef Integer
#undef INT
#undef IPROP
#undef Iname
#undef UPROP
#undef Uname
#undef OK_UI
#undef IMARK

#undef I_MAX
#undef I_MIN
#undef U_MAX
#endif

#ifdef PASS1

/* Define the things we're going to use this pass */

#define Number	float
#define THING	"FLOAT"
#define Thing	"Float"
#define thing	"float"
#define Fname	"FLT"
#define FPROP	fprop
#define Store	fStore
#define Sum	fSum
#define Diff	fDiff
#define Mul	fMul
#define Div	fDiv
#define ZERO	0.0
#define HALF	0.5
#define ONE	1.0
#define TWO	2.0
#define THREE	3.0
#define FOUR	4.0
#define Self	fSelf
#define F_check	fCheck
#define MARK	"F"
#ifdef VERIFY
#define Validate(prec, val, req, same) fValidate(prec, val, req, same)
#endif

#define EPROP	efprop

#define Integer	short
#define INT	"short"
#define IPROP	sprop
#define Iname	"SHRT"
#ifndef NO_UI
#define OK_UI 1
#endif
#define IMARK	""

#define UPROP	usprop
#define Uname	"USHRT"

#ifdef SHRT_MAX
#define I_MAX		SHRT_MAX
#endif
#ifdef SHRT_MIN
#define I_MIN		SHRT_MIN
#endif
#ifdef USHRT_MAX
#define U_MAX		USHRT_MAX
#endif

#ifdef FLT_RADIX
#define F_RADIX		FLT_RADIX
#endif
#ifdef FLT_MANT_DIG
#define F_MANT_DIG	FLT_MANT_DIG
#endif
#ifdef FLT_DIG
#define F_DIG		FLT_DIG
#endif
#ifdef FLT_ROUNDS
#define F_ROUNDS	FLT_ROUNDS
#endif
#ifdef FLT_EPSILON
#define F_EPSILON	FLT_EPSILON
#endif
#ifdef FLT_MIN_EXP
#define F_MIN_EXP	FLT_MIN_EXP
#endif
#ifdef FLT_MIN
#define F_MIN		FLT_MIN
#endif
#ifdef FLT_MIN_10_EXP
#define F_MIN_10_EXP	FLT_MIN_10_EXP
#endif
#ifdef FLT_MAX_EXP
#define F_MAX_EXP	FLT_MAX_EXP
#endif
#ifdef FLT_MAX
#define F_MAX		FLT_MAX
#endif
#ifdef FLT_MAX_10_EXP
#define F_MAX_10_EXP	FLT_MAX_10_EXP
#endif

#endif /* PASS1 */

#ifdef PASS2

#define Number	double
#define THING	"DOUBLE"
#define Thing	"Double"
#define thing	"double"
#define Fname	"DBL"
#define FPROP	dprop
#define Store	dStore
#define Sum	dSum
#define Diff	dDiff
#define Mul	dMul
#define Div	dDiv
#define ZERO	0.0
#define HALF	0.5
#define ONE	1.0
#define TWO	2.0
#define THREE	3.0
#define FOUR	4.0
#define Self	dSelf
#define F_check	dCheck
#define MARK	""
#ifdef VERIFY
#define Validate(prec, val, req, same) dValidate(prec, val, req, same)
#endif

#define EPROP	edprop

#define Integer	int
#define INT	"int"
#define IPROP	iprop
#define Iname	"INT"
#define OK_UI	1 /* Unsigned int is always possible */
#define IMARK	""

#define UPROP	uiprop
#define Uname	"UINT"

#ifdef INT_MAX
#define I_MAX		INT_MAX
#endif
#ifdef INT_MIN
#define I_MIN		INT_MIN
#endif
#ifdef UINT_MAX
#define U_MAX		UINT_MAX
#endif

#ifdef DBL_MANT_DIG
#define F_MANT_DIG	DBL_MANT_DIG
#endif
#ifdef DBL_DIG
#define F_DIG		DBL_DIG
#endif
#ifdef DBL_EPSILON
#define F_EPSILON	DBL_EPSILON
#endif
#ifdef DBL_MIN_EXP
#define F_MIN_EXP	DBL_MIN_EXP
#endif
#ifdef DBL_MIN
#define F_MIN		DBL_MIN
#endif
#ifdef DBL_MIN_10_EXP
#define F_MIN_10_EXP	DBL_MIN_10_EXP
#endif
#ifdef DBL_MAX_EXP
#define F_MAX_EXP	DBL_MAX_EXP
#endif
#ifdef DBL_MAX
#define F_MAX		DBL_MAX
#endif
#ifdef DBL_MAX_10_EXP
#define F_MAX_10_EXP	DBL_MAX_10_EXP
#endif

#endif /* PASS2 */

#ifdef PASS3

#ifdef STDC
#define Number	long double

#define ZERO	0.0L
#define HALF	0.5L
#define ONE	1.0L
#define TWO	2.0L
#define THREE	3.0L
#define FOUR	4.0L
#endif

#define THING	"LONG DOUBLE"
#define Thing	"Long double"
#define thing	"long double"
#define Fname	"LDBL"
#define FPROP	ldprop
#define Store	ldStore
#define Sum	ldSum
#define Diff	ldDiff
#define Mul	ldMul
#define Div	ldDiv
#define Self	ldSelf
#define F_check	ldCheck
#define MARK	"L"
#ifdef VERIFY
#define Validate(prec, val, req, same) ldValidate(prec, val, req, same)
#endif

#define EPROP	eldprop

#define Integer	long
#define INT	"long"
#define IPROP	lprop
#define Iname	"LONG"
#ifndef NO_UI
#define OK_UI	1
#endif
#define IMARK	"L"

#define UPROP	ulprop
#define Uname	"ULONG"

#ifdef LONG_MAX
#define I_MAX	LONG_MAX
#endif
#ifdef LONG_MIN
#define I_MIN	LONG_MIN
#endif
#ifdef ULONG_MAX
#define U_MAX	ULONG_MAX
#endif

#ifdef LDBL_MANT_DIG
#define F_MANT_DIG	LDBL_MANT_DIG
#endif
#ifdef LDBL_DIG
#define F_DIG		LDBL_DIG
#endif
#ifdef LDBL_EPSILON
#define F_EPSILON	LDBL_EPSILON
#endif
#ifdef LDBL_MIN_EXP
#define F_MIN_EXP	LDBL_MIN_EXP
#endif
#ifdef LDBL_MIN
#define F_MIN		LDBL_MIN
#endif
#ifdef LDBL_MIN_10_EXP
#define F_MIN_10_EXP	LDBL_MIN_10_EXP
#endif
#ifdef LDBL_MAX_EXP
#define F_MAX_EXP	LDBL_MAX_EXP
#endif
#ifdef LDBL_MAX
#define F_MAX		LDBL_MAX
#endif
#ifdef LDBL_MAX_10_EXP
#define F_MAX_10_EXP	LDBL_MAX_10_EXP
#endif

#endif /* PASS3 */

#define UNDEFINED (-2)

#ifndef I_MAX
#define I_MAX	((unsigned long) UNDEFINED)
#endif
#ifndef I_MIN
#define I_MIN	((unsigned long) UNDEFINED)
#endif
#ifndef U_MAX
#define U_MAX	((unsigned long) UNDEFINED)
#endif

#ifndef F_RADIX
#define F_RADIX		UNDEFINED
#endif
#ifndef F_MANT_DIG
#define F_MANT_DIG	UNDEFINED
#endif
#ifndef F_DIG
#define F_DIG		UNDEFINED
#endif
#ifndef F_ROUNDS
#define F_ROUNDS	UNDEFINED
#endif
#ifndef F_EPSILON
#define F_EPSILON	((Number) UNDEFINED)
#endif
#ifndef F_MIN_EXP
#define F_MIN_EXP	UNDEFINED
#endif
#ifndef F_MIN
#define F_MIN		((Number) UNDEFINED)
#endif
#ifndef F_MIN_10_EXP
#define F_MIN_10_EXP	UNDEFINED
#endif
#ifndef F_MAX_EXP
#define F_MAX_EXP	UNDEFINED
#endif
#ifndef F_MAX
#define F_MAX		((Number) UNDEFINED)
#endif
#ifndef F_MAX_10_EXP
#define F_MAX_10_EXP	UNDEFINED
#endif

#ifndef VERIFY
#define Validate(prec, val, req, same) {;}
#endif

#ifdef Integer

Procedure IPROP() {
	/* the properties of short, int, and long */
	Volatile Integer newi, int_max, maxeri, int_min, minneri;
	Volatile int ibits, ipower, two=2;

	/* Calculate max short/int/long ***********************************/
	/* Calculate 2**n-1 until overflow - then use the previous value  */

	newi=1; int_max=0;

	if (setjmp(lab)==0) { /* Yields int_max */
		for(ipower=0; newi>int_max; ipower++) {
			int_max=newi;
			newi=newi*two+1;
		}
		Vprintf("%sOverflow of a%s %s does not generate a trap%s\n",
			co, INT[0]=='i'?"n":"", INT, oc);
	} else {
		Vprintf("%sOverflow of a%s %s generates a trap%s\n",
			co, INT[0]=='i'?"n":"", INT, oc);
	}
	Unexpected(7);

	/* Minimum value: assume either two's or one's complement *********/
	int_min= -int_max;
	if (setjmp(lab)==0) { /* Yields int_min */
		if (int_min-1 < int_min) int_min--;
	}
	Unexpected(8);

	/* Now for those daft Cybers */

	maxeri=0; newi=int_max;

	if (setjmp(lab)==0) { /* Yields maxeri */
		for(ibits=ipower; newi>maxeri; ibits++) {
			maxeri=newi;
			newi=newi+newi+1;
		}
	}
	Unexpected(9);

	minneri= -maxeri;
	if (setjmp(lab)==0) { /* Yields minneri */
		if (minneri-1 < minneri) minneri--;
	}
	Unexpected(10);

	Vprintf("%sMaximum %s = %ld (= 2**%d-1)%s\n",
		co, INT, (long)int_max, ipower, oc);
	Vprintf("%sMinimum %s = %ld%s\n", co, INT, (long)int_min, oc);

	if (L) i_define(D_INT_MAX, INT, Iname, "_MAX",
			(long) int_max, 0L,
			(long) I_MAX, IMARK);
	if (L) i_define(D_INT_MIN, INT, Iname, "_MIN",
			(long) int_min, (long) (PASS==1?maxint:int_max),
			(long) I_MIN, IMARK);

	if(int_max < 0) { /* It has happened */
		eek_a_bug("signed integral comparison faulty?");
	}

	if (maxeri>int_max) {
		Vprintf("%sThere is a larger %s, %ld (= 2**%d-1), %s %s%s\n",
			co, INT, (long)maxeri, ibits,
			"but only for addition, not multiplication",
			"(I smell a Cyber!)",
			oc);
	}

	if (minneri<int_min) {
		Vprintf("%sThere is a smaller %s, %ld, %s %s%s\n",
			co, INT, (long)minneri,
			"but only for addition, not multiplication",
			"(I smell a Cyber!)",
			oc);
	}
}

Procedure UPROP () {
	/* The properties of unsigned short/int/long */
#ifdef OK_UI
	Volatile unsigned Integer u_max, newi, two;
	newi=1; u_max=0; two=2;

	if (setjmp(lab)==0) { /* Yields u_max */
		while(newi>u_max) {
			u_max=newi;
			newi=newi*two+1;
		}
	}
	Unexpected(11);
	Vprintf("%sMaximum unsigned %s = %lu%s\n",
		co, INT, (unsigned long) u_max, oc);

	/* Oh woe: new standard C defines value preserving promotions */
	if (L) {
		if (PASS == 1 && sizeof(short) < sizeof(int)) {
			/* Special only for short */
			i_define(D_UINT_MAX, INT, Uname, "_MAX",
				 (unsigned long) u_max, 0L,
				 (unsigned long) U_MAX, IMARK);
		} else {
			u_define(D_UINT_MAX, INT, Uname, "_MAX",
				 (unsigned long) u_max,
				 (unsigned long) U_MAX, IMARK);
		}
	}
#endif
}

#endif /* Integer */

#ifdef Number

/* The following routines are intended to defeat any attempt at optimisation
   or use of extended precision, and to defeat faulty narrowing casts.
   The weird prototypes are because of widening incompatibilities.
*/
#ifdef STDC
#define ARGS1(atype, a) (atype a)
#define ARGS2(atype, a, btype, b) (atype a, btype b)
#else
#define ARGS1(atype, a) (a) atype a;
#define ARGS2(atype, a, btype, b) (a, b) atype a; btype b;
#endif

Procedure Store ARGS2(Number, a, Number *, b) { *b=a; }
Number Sum ARGS2(Number, a, Number, b) {Number r; Store(a+b, &r); return (r); }
Number Diff ARGS2(Number, a, Number, b){Number r; Store(a-b, &r); return (r); }
Number Mul ARGS2(Number, a, Number, b) {Number r; Store(a*b, &r); return (r); }
Number Div ARGS2(Number, a, Number, b) {Number r; Store(a/b, &r); return (r); }
Number Self ARGS1(Number, a)	       {Number r; Store(a,   &r); return (r); }

Procedure F_check ARGS((int precision, Long_double val1));

Procedure F_check(precision, val1) int precision; Long_double val1; {
	/* You don't think I'm going to go to all the trouble of writing
	   a program that works out what all sorts of values are, only to
	   have printf go and print the wrong values out, do you?
	   No, you're right, so this function tries to see if printf
	   has written the right value, by reading it back again.
	   This introduces a new problem of course: suppose printf writes
	   the correct value, and scanf reads it back wrong... oh well.
	   But I'm adamant about this: the precision given is enough
	   to uniquely identify the printed number, therefore I insist
	   that sscanf read the number back identically. Harsh yes, but
	   sometimes you've got to be cruel to be kind.
	*/
	Number val, new, diff;
	double rem;
	int e;
	char *rep;
	char *f2;

#ifdef NO_LONG_DOUBLE_IO
	double new1;
	/* On the Sun 3, sscanf clobbers 4 words,
	   which leads to a crash when this function tries to return.  */
	f2= "%le";   /* Input */
	/* It is no use checking long doubles if we can't
	   read and write them.  */
	if (sizeof (Number) > sizeof(double))
	  return;
#else
	Long_double new1;
	if (sizeof(double) == sizeof(Long_double)) {
		/* Assume they're the same, and use non-stdc format */
		/* This is for stdc compilers using non-stdc libraries */
		f2= "%le";   /* Input */
	} else {
		/* It had better support Le then */
		f2= "%Le";
	}
#endif
	val= val1;
	rep= f_rep(precision, (Long_double) val);
	if (setjmp(lab)==0) {
		sscanf(rep, f2, &new1);
	} else {
		eek_a_bug("sscanf caused a trap");
		printf("%s    scanning: %s format: %s%s\n\n", co, rep, f2, oc);
		Unexpected(12);
		return;
	}

	if (setjmp(lab)==0) { /* See if new is usable */
		new= new1;
		if (new != 0.0) {
			diff= val/new - 1.0;
			if (diff < 0.1) diff= 1.0;
			/* That should be enough to generate a trap */
		}
	} else {
		eek_a_bug("sscanf returned an unusable number");
		printf("%s    scanning: %s with format: %s%s\n\n",
		       co, rep, f2, oc);
		Unexpected(13);
		return;
	}

	Unexpected(14);
	if (new != val) {
		eek_a_bug("Possibly bad output from printf above");
		if (!exponent((Long_double)val, &rem, &e)) {
			printf("%s    but value was an unusable number%s\n\n",
			       co, oc);
			return;
		}
		printf("%s    expected value around %.*fe%d, bit pattern:\n    ",
		       co, precision, rem, e);
		bitpattern((char *) &val, (unsigned)sizeof(val));
		printf ("%s\n", oc);
		printf("%s    sscanf gave           %s, bit pattern:\n    ",
		       co, f_rep(precision, (Long_double) new));
		bitpattern((char *) &new, (unsigned)sizeof(new));
		printf ("%s\n", oc);
		if (setjmp(lab) == 0) {
			diff= val-new;
			printf("%s    difference= %s%s\n\n",
			       co, f_rep(precision, (Long_double) diff), oc);
		} /* else forget it */
		Unexpected(15);
	}
}

#ifdef VERIFY
Procedure Validate(prec, val, req, same) int prec, same; Long_double val, req; {
	/* Check that the compiler has read a #define value correctly */
	Unexpected(16);
	if (!same) {
		printf("%s*** Verify failed for above #define!\n", co);
		if (setjmp(lab) == 0) { /* for the case that req == nan */
			printf("       Compiler has %s for value%s\n",
			       f_rep(prec, req), oc);
		} else {
			printf("       Compiler has %s for value%s\n",
			       "an unusable number", oc);
		}
		if (setjmp(lab) == 0) {
			F_check(prec, (Long_double) req);
		} /*else forget it*/
		if (setjmp(lab) == 0) {
			if (req > 0.0 && val > 0.0) {
				printf("%s    difference= %s%s\n",
				       co, f_rep(prec, val-req), oc);
			}
		} /*else forget it*/
		Unexpected(17);
		printf("\n");
		bugs++;
	} else if (val != req) {
		if (stdc) eek_a_bug("constant has the wrong precision");
		else eek_a_bug("the cast didn't work");
		printf("\n");
	}
}
#endif /* VERIFY */

int FPROP(bits_per_byte) int bits_per_byte; {
	/* Properties of floating types, using algorithms by Cody and Waite
	   from MA Malcolm, as modified by WM Gentleman and SB Marovich.
	   Further extended by S Pemberton.

	   Returns the number of digits in the fraction.
	*/

	Volatile int
		i, f_radix, iexp, irnd, mrnd, f_rounds, f_mant_dig,
		iz, k, inf, machep, f_max_exp, f_min_exp, mx, negeps,
		mantbits, digs, f_dig, trap,
		hidden, normal, f_min_10_exp, f_max_10_exp;
	Volatile Number
		a, b, base, basein, basem1, f_epsilon, epsneg,
		eps, epsp1, etop, ebot,
		f_max, newxmax, f_min, xminner, y, y1, z, z1, z2;

	Unexpected(18);

	Vprintf("%sPROPERTIES OF %s%s\n", co, THING, oc);

	/* Base and size of significand **************************************/
	/* First repeatedly double until adding 1 has no effect.	  */
	/* For instance, if base is 10, with 3 significant digits	  */
	/* it will try 1, 2, 4, 8, ... 512, 1024, and stop there,	  */
	/* since 1024 is only representable as 1020.			  */
	a=1.0;
	if (setjmp(lab)==0) { /* inexact trap? */
		do { a=Sum(a, a); }
		while (Diff(Diff(Sum(a, ONE), a), ONE) == ZERO);
	} else {
		fprintf(stderr, "*** Program got loss-of-precision trap!\n");
		/* And supporting those is just TOO much trouble! */
		farewell(bugs+1);
	}
	Unexpected(19);
	/* Now double until you find a number that can be added to the	  */
	/* above number. For 1020 this is 8 or 16, depending whether the  */
	/* result is rounded or truncated.				  */
	/* In either case the result is 1030. 1030-1020= the base, 10.	  */
	b=1.0;
	do { b=Sum(b, b); } while ((base=Diff(Sum(a, b), a)) == ZERO);
	f_radix=base;
	Vprintf("%sBase = %d%s\n", co, f_radix, oc);

	/* Sanity check; if base<2, I can't guarantee the rest will work  */
	if (f_radix < 2) {
		eek_a_bug("Function return or parameter passing faulty? (This is a guess.)");
		printf("\n");
		return(0);
	}

	if (PASS == 1) { /* only for FLT */
		flt_radix= f_radix;
		if (F) i_define(D_FLT_RADIX, "", "FLT", "_RADIX",
				(long) f_radix, 0L, (long) F_RADIX, "");
	} else if (f_radix != flt_radix) {
		printf("\n%s*** WARNING: %s %s (%d) %s%s\n",
		       co, thing, "arithmetic has a different radix",
		       f_radix, "from float", oc);
		bugs++;
	}

	/* Now the number of digits precision */
	f_mant_dig=0; b=1.0;
	do { f_mant_dig++; b=Mul(b, base); }
	while (Diff(Diff(Sum(b, ONE), b), ONE) == ZERO);
	f_dig=floor_log(10, (Long_double)(b/base)) + (base==10?1:0);
	Vprintf("%sSignificant base digits = %d %s %d %s%s\n",
		co, f_mant_dig, "(= at least", f_dig, "decimal digits)", oc);
	if (F) i_define(D_MANT_DIG, thing, Fname, "_MANT_DIG",
			(long) f_mant_dig, 0L, (long) F_MANT_DIG, "");
	if (F) i_define(D_DIG, thing, Fname, "_DIG",
			(long) f_dig, 0L, (long) F_DIG, "");
	digs= ceil_log(10, (Long_double)b); /* the number of digits to printf */

	/* Rounding *******************************************************/
	basem1=Diff(base, HALF);
	if (Diff(Sum(a, basem1), a) != ZERO) {
		if (f_radix == 2) basem1=0.375;
		else basem1=1.0;
		if (Diff(Sum(a, basem1), a) != ZERO) irnd=2; /* away from 0 */
		else irnd=1; /* to nearest */
	} else irnd=0; /* towards 0 */

	basem1=Diff(base, HALF);

	if (Diff(Diff(-a, basem1), -a) != ZERO) {
		if (f_radix == 2) basem1=0.375;
		else basem1=1.0;
		if (Diff(Diff(-a, basem1), -a) != ZERO) mrnd=2; /* away from 0*/
		else mrnd=1; /* to nearest */
	} else mrnd=0; /* towards 0 */

	f_rounds= -1; /* Unknown rounding */
	if (irnd==0 && mrnd==0) f_rounds=0; /* zero = chops */
	if (irnd==1 && mrnd==1) f_rounds=1; /* nearest */
	if (irnd==2 && mrnd==0) f_rounds=2; /* +inf */
	if (irnd==0 && mrnd==2) f_rounds=3; /* -inf */

	if (f_rounds != -1) {
		Vprintf("%sArithmetic rounds towards ", co);
		switch (f_rounds) {
		      case 0: Vprintf("zero (i.e. it chops)"); break;
		      case 1: Vprintf("nearest"); break;
		      case 2: Vprintf("+infinity"); break;
		      case 3: Vprintf("-infinity"); break;
		      default: Vprintf("???"); break;
		}
		Vprintf("%s\n", oc);
	} else { /* Hmm, try to give some help here */
		Vprintf("%sArithmetic rounds oddly: %s\n", co, oc);
		Vprintf("%s    Negative numbers %s%s\n",
			co, mrnd==0 ? "towards zero" :
			    mrnd==1 ? "to nearest" :
				      "away from zero",
			oc);
		Vprintf("%s    Positive numbers %s%s\n",
			co, irnd==0 ? "towards zero" :
			    irnd==1 ? "to nearest" :
				      "away from zero",
			oc);
	}
	/* An extra goody */
	if (f_radix == 2 && f_rounds == 1) {
		if (Diff(Sum(a, ONE), a) != ZERO) {
			Vprintf("%s   Tie breaking rounds up%s\n", co, oc);
		} else if (Diff(Sum(a, THREE), a) == FOUR) {
			Vprintf("%s   Tie breaking rounds to even%s\n", co, oc);
		} else {
			Vprintf("%s   Tie breaking rounds down%s\n", co, oc);
		}
	}
	if (PASS == 1) { /* only for FLT */
		flt_rounds= f_rounds;
		/* Prefer system float.h definition of F_ROUNDS,
		   since it's more likely to be right than our "1".  */
		if (F && (!SYS_FLOAT_H_WRAP || F_ROUNDS == UNDEFINED))
		  i_define(D_FLT_ROUNDS, "", "FLT", "_ROUNDS",
			   (long) f_rounds, 1L, (long) F_ROUNDS, "");
	} else if (f_rounds != flt_rounds) {
		printf("\n%s*** WARNING: %s %s (%d) %s%s\n",
		       co, thing, "arithmetic rounds differently",
		       f_rounds, "from float", oc);
		bugs++;
	}

	/* Various flavours of epsilon ************************************/
	negeps=f_mant_dig+f_mant_dig;
	basein=1.0/base;
	a=1.0;
	for(i=1; i<=negeps; i++) a*=basein;

	b=a;
	while (Diff(Diff(ONE, a), ONE) == ZERO) {
		a*=base;
		negeps--;
	}
	negeps= -negeps;
	Vprintf("%sSmallest x such that 1.0-base**x != 1.0 = %d%s\n",
		co, negeps, oc);

	etop = ONE;
	ebot = ZERO;
	eps = Sum(ebot, Div(Diff(etop, ebot), TWO));
	/* find the smallest epsneg (1-epsneg != 1) by binary search.
	   ebot and etop are the current bounds */
	while (eps != ebot && eps != etop) {
		epsp1 = Diff(ONE, eps);
		if (epsp1 < ONE) etop = eps;
		else ebot = eps;
		eps = Sum(ebot, Div(Diff(etop, ebot), TWO));
	}
	eps= etop;
	/* Sanity check */
	if (Diff(ONE, etop) >= ONE || Diff(ONE, ebot) != ONE) {
		eek_a_bug("internal error calculating epsneg");
	}
	Vprintf("%sSmallest x such that 1.0-x != 1.0 = %s%s\n",
		co, f_rep(digs, (Long_double) eps), oc);
	if (V) F_check(digs, (Long_double) eps);

	epsneg=a;
	if ((f_radix!=2) && irnd) {
	/*	a=(a*(1.0+a))/(1.0+1.0); => */
		a=Div(Mul(a, Sum(ONE, a)), Sum(ONE, ONE));
	/*	if ((1.0-a)-1.0 != 0.0) epsneg=a; => */
		if (Diff(Diff(ONE, a), ONE) != ZERO) epsneg=a;
	}
	/* epsneg is used later */
	Unexpected(20);

	machep= -f_mant_dig-f_mant_dig;
	a=b;
	while (Diff(Sum(ONE, a), ONE) == ZERO) { a*=base; machep++; }
	Vprintf("%sSmallest x such that 1.0+base**x != 1.0 = %d%s\n",
		co, machep, oc);

	etop = ONE;
	ebot = ZERO;
	eps = Sum(ebot, Div(Diff(etop, ebot), TWO));
	/* find the smallest eps (1+eps != 1) by binary search.
	   ebot and etop are the current bounds */
	while (eps != ebot && eps != etop) {
		epsp1 = Sum(ONE, eps);
		if (epsp1 > ONE) etop = eps;
		else ebot = eps;
		eps = Sum(ebot, Div(Diff(etop, ebot), TWO));
	}
	/* Sanity check */
	if (Sum(ONE, etop) <= ONE || Sum(ONE, ebot) != ONE) {
		eek_a_bug("internal error calculating eps");
	}
	f_epsilon=etop;

	Vprintf("%sSmallest x such that 1.0+x != 1.0 = %s%s\n",
		co, f_rep(digs, (Long_double) f_epsilon), oc);

	f_epsilon= Diff(Sum(ONE, f_epsilon), ONE); /* New C standard defn */
	Vprintf("%s(Above number + 1.0) - 1.0 = %s%s\n",
		co, f_rep(digs, (Long_double) (f_epsilon)), oc);

	/* Possible loss of precision warnings here from non-stdc compilers */
	if (F) f_define(D_EPSILON, thing,
			Fname, "_EPSILON", digs,
			(Long_double) f_epsilon,
			(Long_double) F_EPSILON, MARK);
	if (V || F) F_check(digs, (Long_double) f_epsilon);
	Unexpected(21);
	if (F) Validate(digs, (Long_double) f_epsilon, (Long_double) F_EPSILON,
			f_epsilon == Self(F_EPSILON));
	Unexpected(22);

	/* Extra chop info *************************************************/
	if (f_rounds == 0) {
		if (Diff(Mul(Sum(ONE,f_epsilon),ONE),ONE) !=  ZERO) {
			Vprintf("%sAlthough arithmetic chops, it uses guard digits%s\n", co, oc);
		}
	}

	/* Size of and minimum normalised exponent ************************/
	y=0; i=0; k=1; z=basein; z1=(1.0+f_epsilon)/base;

	/* Coarse search for the largest power of two */
	if (setjmp(lab)==0) { /* for underflow trap */ /* Yields i, k, y, y1 */
		do {
			y=z; y1=z1;
			z=Mul(y,y); z1=Mul(z1, y);
			a=Mul(z,ONE);
			z2=Div(z1,y);
			if (z2 != y1) break;
			if ((Sum(a,a) == ZERO) || (fabs(z) >= y)) break;
			i++;
			k+=k;
		} while(1);
	} else {
		Vprintf("%s%s underflow generates a trap%s\n", co, Thing, oc);
	}
	Unexpected(23);

	if (f_radix != 10) {
		iexp=i+1; /* for the sign */
		mx=k+k;
	} else {
		iexp=2;
		iz=f_radix;
		while (k >= iz) { iz*=f_radix; iexp++; }
		mx=iz+iz-1;
	}

	/* Fine tune starting with y and y1 */
	if (setjmp(lab)==0) { /* for underflow trap */ /* Yields k, f_min */
		do {
			f_min=y; z1=y1;
			y=Div(y,base); y1=Div(y1,base);
			a=Mul(y,ONE);
			z2=Mul(y1,base);
			if (z2 != z1) break;
			if ((Sum(a,a) == ZERO) || (fabs(y) >= f_min)) break;
			k++;
		} while (1);
	}
	Unexpected(24);

	f_min_exp=(-k)+1;

	if ((mx <= k+k-3) && (f_radix != 10)) { mx+=mx; iexp+=1; }
	Vprintf("%sNumber of bits used for exponent = %d%s\n", co, iexp, oc);
	Vprintf("%sMinimum normalised exponent = %d%s\n", co, f_min_exp-1, oc);
	if (F)
	  i_define(D_MIN_EXP, thing, Fname, "_MIN_EXP",
		   (long) f_min_exp, (long) maxint, (long) F_MIN_EXP, "");

	if (setjmp(lab)==0) {
		Vprintf("%sMinimum normalised positive number = %s%s\n",
			co, f_rep(digs, (Long_double) f_min), oc);
	} else {
		eek_a_bug("printf can't print the smallest normalised number");
		printf("\n");
	}
	Unexpected(25);
	/* Possible loss of precision warnings here from non-stdc compilers */
	if (setjmp(lab) == 0) {
		if (F) f_define(D_MIN, thing,
				Fname, "_MIN", digs,
				(Long_double) f_min,
				(Long_double) F_MIN, MARK);
		if (V || F) F_check(digs, (Long_double) f_min);
	} else {
		eek_a_bug("xxx_MIN caused a trap");
		printf("\n");
	}

	if (setjmp(lab) == 0) {
		if (F) Validate(digs, (Long_double) f_min, (Long_double) F_MIN,
				f_min == Self(F_MIN));
	} else {
		printf("%s*** Verify failed for above #define!\n    %s %s\n\n",
		       co, "Compiler has an unusable number for value", oc);
		bugs++;
	}
	Unexpected(26);

	a=1.0; f_min_10_exp=0;
	while (a > f_min*10.0) { a/=10.0; f_min_10_exp--; }
	if (F) i_define(D_MIN_10_EXP, thing, Fname, "_MIN_10_EXP",
			(long) f_min_10_exp, (long) maxint,
			(long) F_MIN_10_EXP, "");

	/* Minimum exponent ************************************************/
	if (setjmp(lab)==0) { /* for underflow trap */ /* Yields xminner */
		do {
			xminner=y;
			y=Div(y,base);
			a=Mul(y,ONE);
			if ((Sum(a,a) == ZERO) || (fabs(y) >= xminner)) break;
		} while (1);
	}
	Unexpected(27);

	if (xminner != 0.0 && xminner != f_min) {
		normal= 0;
		Vprintf("%sThe smallest numbers are not kept normalised%s\n",
			co, oc);
		if (setjmp(lab)==0) {
		    Vprintf("%sSmallest unnormalised positive number = %s%s\n",
			    co, f_rep(digs, (Long_double) xminner), oc);
		    if (V) F_check(digs, (Long_double) xminner);
		} else {
			eek_a_bug("printf can't print the smallest unnormalised number.");
			printf("\n");
		}
		Unexpected(28);
	} else {
		normal= 1;
		Vprintf("%sThe smallest numbers are normalised%s\n", co, oc);
	}

	/* Maximum exponent ************************************************/
	f_max_exp=2; f_max=1.0; newxmax=base+1.0;
	inf=0; trap=0;
	while (f_max<newxmax) {
		f_max=newxmax;
		if (setjmp(lab) == 0) { /* Yields inf, f_max_exp */
			newxmax=Mul(newxmax, base);
		} else {
			trap=1;
			break;
		}
		if (Div(newxmax, base) != f_max) {
			inf=1; /* ieee infinity */
			break;
		}
		f_max_exp++;
	}
	Unexpected(29);
	if (trap) {
		Vprintf("%s%s overflow generates a trap%s\n", co, Thing, oc);
	}

	if (inf) Vprintf("%sThere is an 'infinite' value%s\n", co, oc);
	Vprintf("%sMaximum exponent = %d%s\n", co, f_max_exp, oc);
	if (F) i_define(D_MAX_EXP, thing, Fname, "_MAX_EXP",
			(long) f_max_exp, 0L, (long) F_MAX_EXP, "");

	/* Largest number ***************************************************/
	f_max=Diff(ONE, epsneg);
	if (Mul(f_max,ONE) != f_max) f_max=Diff(ONE, Mul(base,epsneg));
	for (i=1; i<=f_max_exp; i++) f_max=Mul(f_max, base);

	if (setjmp(lab)==0) {
		Vprintf("%sMaximum number = %s%s\n",
			co, f_rep(digs, (Long_double) f_max), oc);
	} else {
		eek_a_bug("printf can't print the largest double.");
		printf("\n");
	}
	if (setjmp(lab)==0) {
	/* Possible loss of precision warnings here from non-stdc compilers */
		if (F) f_define(D_MAX, thing,
				Fname, "_MAX", digs,
				(Long_double) f_max,
				(Long_double) F_MAX, MARK);
		if (V || F) F_check(digs, (Long_double) f_max);
	} else {
		eek_a_bug("xxx_MAX caused a trap");
		printf("\n");
	}
	if (setjmp(lab)==0) {
		if (F) Validate(digs, (Long_double) f_max, (Long_double) F_MAX,
				f_max == Self(F_MAX));
	} else {
		printf("%s*** Verify failed for above #define!\n    %s %s\n\n",
		       co, "Compiler has an unusable number for value", oc);
		bugs++;
	}
	Unexpected(30);

	a=1.0; f_max_10_exp=0;
	while (a < f_max/10.0) { a*=10.0; f_max_10_exp++; }
	if (F) i_define(D_MAX_10_EXP, thing, Fname, "_MAX_10_EXP",
			(long) f_max_10_exp, 0L, (long) F_MAX_10_EXP, "");

	/* Hidden bit + sanity check ****************************************/
	if (f_radix != 10) {
		hidden=0;
		mantbits=floor_log(2, (Long_double)f_radix)*f_mant_dig;
		if (mantbits == 64
		    && iexp == 15
		    && f_max_exp+f_min_exp > 0 /* ??? f_min_exp may be wrong.  */
		    && mantbits+iexp+17 == (int)sizeof(Number)*bits_per_byte) {
			Vprintf("%sArithmetic probably doesn't use a hidden bit%s\n", co, oc);
			Vprintf("%sIt's probably 80387 or 68881 extended real%s\n", co, oc);
			goto is_extended;
		}
		if (mantbits+iexp == (int)sizeof(Number)*bits_per_byte) {
			hidden=1;
			Vprintf("%sArithmetic uses a hidden bit%s\n", co, oc);
		} else if (mantbits+iexp+1 == (int)sizeof(Number)*bits_per_byte) {
			Vprintf("%sArithmetic doesn't use a hidden bit%s\n",
				co, oc);
		} else {
			printf("\n%s%s\n    %s %s %s!%s\n\n",
			       co,
			       "*** Something fishy here!",
			       "Exponent size + significand size doesn't match",
			       "with the size of a", thing,
			       oc);
		}
		if (hidden && f_radix == 2 && f_max_exp+f_min_exp==3) {
			Vprintf("%sIt looks like %s length IEEE format%s\n",
				co, f_mant_dig==24 ? "single" :
				    f_mant_dig==53 ? "double" :
				    f_mant_dig >53 ? "extended" :
						"some", oc);
is_extended:
			if (f_rounds != 1 || normal) {
				Vprintf("%s   though ", co);
				if (f_rounds != 1) {
					Vprintf("the rounding is unusual");
					if (normal) Vprintf(" and ");
				}
				if (normal) Vprintf("the normalisation is unusual");
				Vprintf("%s\n", oc);
			}
		} else {
			Vprintf("%sIt doesn't look like IEEE format%s\n",
				co, oc);
		}
	}
	printf("\n"); /* regardless of verbosity */
	return f_mant_dig;
}

Procedure EPROP(fprec, dprec, lprec) int fprec, dprec, lprec; {
	/* See if expressions are evaluated in extended precision.
	   Some compilers optimise even if you don't want it,
	   and then this function fails to produce the right result.
	   We try to diagnose this if it happens.
	*/
	Volatile int eprec;
	Volatile double a, b, base, old;
	Volatile Number d, oldd, dbase, one, zero;
	Volatile int bad=0;

	/* Size of significand **************************************/
	a=1.0;
	if (setjmp(lab) == 0) { /* Yields nothing */
		do { old=a; a=a+a; }
		while ((((a+1.0)-a)-1.0) == 0.0 && a>old);
	} else bad=1;

	/* Avoid the comparison if bad is set,
	   to avoid trouble on the convex.  */
	if (!bad && (a <= old)) bad=1;

	if (!bad) {
		b=1.0;
		if (setjmp(lab) == 0) { /* Yields nothing */
			do { old=b; b=b+b; }
			while ((base=((a+b)-a)) == 0.0 && b>old);
			if (b <= old) bad=1;
		} else bad=1;
	}

	if (!bad) {
		eprec=0; d=1.0; dbase=base; one=1.0; zero=0.0;
		if (setjmp(lab) == 0) { /* Yields nothing */
			do { eprec++; oldd=d; d=d*dbase; }
			while ((((d+one)-d)-one) == zero && d>oldd);
			if (d <= oldd) bad=1;
		} else bad=1;
	}

	Unexpected(31);

	if (bad) {
	  Vprintf("%sCan't determine precision for %s expressions:\n%s%s\n",
		 co, thing, "   check that you compiled without optimisation!",
		 oc);
	} else if (eprec==dprec) {
	  Vprintf("%s%s expressions are evaluated in double precision%s\n",
		  co, Thing, oc);
	} else if (eprec==fprec) {
	  Vprintf("%s%s expressions are evaluated in float precision%s\n",
		  co, Thing, oc);
	} else if (eprec==lprec) {
	  Vprintf("%s%s expressions are evaluated in long double precision%s\n",
		  co, Thing, oc);
	} else {
		Vprintf("%s%s expressions are evaluated in a %s %s %d %s%s\n",
			co, Thing, eprec>dprec ? "higher" : "lower",
			"precision than double,\n   using",
			eprec, "base digits",
		        oc);
	}
}

#else /* not Number */

#ifdef FPROP /* Then create dummy routines for long double */
/* ARGSUSED */
int FPROP(bits_per_byte) int bits_per_byte; { return 0; }
#endif
#ifdef EPROP
/* ARGSUSED */
Procedure EPROP(fprec, dprec, lprec) int fprec, dprec, lprec; {}
#endif

#endif /* ifdef Number */

/* Increment the pass number */
#undef PASS

#ifdef PASS2
#undef PASS2
#define PASS 3
#define PASS3 1
#endif

#ifdef PASS1
#undef PASS1
#define PASS 2
#define PASS2 1
#endif

#ifdef PASS0
#undef PASS0
#endif

#ifdef PASS /* then rescan this file */
#ifdef NO_FILE
#include "enquire.c"
#else
#include FILENAME  /* if this line fails to compile, define NO_FILE */
#endif
#endif /* PASS */

