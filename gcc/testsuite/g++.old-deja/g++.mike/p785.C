// Special g++ Options: -w
// Build don't link:
// prms-id: 785

//# 1 "GctNameRef.List.cc"
//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine++.h" 1
// -*- C++ -*-
#ifndef FALSE
#define FALSE false
#endif
#ifndef TRUE
#define TRUE true
#endif


//
// WATCHOUT - CC 2.1 uses regular old cpp as its #ifdef processor
// whereas GNU uses a special preprocessor (actually ``gcc-cpp -+'')
// This implies that there can be no C++ comments on lines which are
// to be understood by cpp.   Actually it turns out that only lines
// with grammatical structures (such as ``#if defined( ... )'') are
// affected, but this is probably a good rule to follow elsewhere too.
//

//
// Define a ``Standard C++ Unix Machine''
//
// By whatever means are available in sumachine and elsewhere,
// figure out what type of C++ world we are on.
//
// See also "sumachine.h"
//
//
// This file is expected to be included as the first #include file in
// all .cc files This file should be included in each and every src file
// compiled b/c it ensures that the environment which those src files
// expect exists either by fiat or by faking it.
//


//
// Its GNU C++
//
// This pragma only works under g++ 1 (it no longer works for gcc2)




// Because between releases of Cygnus' stuff, the definitions keep bouncing
// around between  so fast that one can't
// keep one's code compiling ...  Just include them all and be done with it.
//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdlib.h" 1





//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1


extern "C" {



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/mips/lib/gcc/decstatn/cygnus-1.96/include/stddef.h" 1






/* This avoids lossage on Sunos but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE__TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */

/* Signed type of difference of two pointers.  */














typedef long int ptrdiff_t;






/* Unsigned type of `sizeof' something.  */














typedef int size_t;






/* Data type for wide chars.  */














typedef int wchar_t;








/* A null pointer constant.  */




/* Offset of member MEMBER in a struct of type TYPE.  */





//# 7 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 2




}

//# 6 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdlib.h" 2


extern "C" {

int       abs(int);


void abort(void);




double    atof(const char*);
int       atoi(const char*);
long      atol(const char*);

int       atexit(auto void (*p) (void));
int       bsearch (const void *, const void *, size_t,
                   size_t, auto int (*ptf)(const void*, const void*));
void*     calloc(size_t, size_t);
void      cfree(void*);


void exit(int);




char*     fcvt(double, int, int*, int*);
void      free(void*);
char*     getenv(const char*);
int       getopt(int, const char**, const char*);
int       getpw(int, char*);
char*     gcvt(double, int, char*);
char*     ecvt(double, int, int*, int*);
extern char**   environ;

long      labs(long);
void*     malloc(size_t);
size_t    malloc_usable_size(void*);
int       putenv(const char*);
extern char*    optarg;
extern int      opterr;
extern int      optind;
void      qsort(void*, size_t, size_t, auto int (*ptf)(void*,void*));
int       rand(void);
void*     realloc(void*, size_t);
int       setkey(const char*);
int       srand(unsigned int);
double    strtod(const char*, char**);
long      strtol(const char*, char**, int);
unsigned long stroul(const char**, int);
int       system(const char*);

long      random(void);
void      srandom(int);
char*     setstate(char*);
char*     initstate(unsigned, char*, int);

double    drand48(void);
void      lcong48(short*);
long      jrand48(short*);
long      lrand48(void);
long      mrand48(void);
long      nrand48(short*);
short*    seed48(short*);
void      srand48(long);

char*     ctermid(char*);
char*     cuserid(char*);
char*     tempnam(const char*, const char*);
char*     tmpnam(char*);

}

//# 44 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine++.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988, 1992 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/





//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1
//# 12 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h"

//# 23 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdlib.h" 1


//# 80 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdlib.h"

//# 24 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/string.h" 1




//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1
//# 12 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h"

//# 5 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/string.h" 2



extern "C" {

char*     strcat(char*, const char*);
char*     strchr(const char*, int);
int       strcmp(const char*, const char*);
int       strcoll(const char*, const char*);
char*     strcpy(char*, const char*);
size_t    strcspn(const char*, const char*);
char*     strdup(const char*);

char*     strncat(char*, const char*, size_t);
int       strncmp(const char*, const char*, size_t);
char*     strncpy(char*, const char*, size_t);
char*     strpbrk(const char*, const char*);
char*     strrchr(const char*, int);
size_t    strspn(const char*, const char*);
char*     strstr(const char*, const char *);
char*     strtok(char*, const char*);
size_t    strxfrm(char*, const char*, size_t);

}







extern "C" {
char*     index(const char*, int);
char*     rindex(const char*, int);
}



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/memory.h" 1




//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1
//# 12 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h"

//# 5 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/memory.h" 2


extern "C" {

void*     memalign(size_t, size_t);
void*     memccpy(void*, const void*, int, size_t);
void*     memchr(const void*, int, size_t);


void*     memset(void*, int, size_t);
int       ffs(int);
size_t    getpagesize(void);
void*     valloc(size_t);

}









extern "C" {
void      bcopy(const void*, void*, size_t); // USG uses version in bcopy.c
int       bcmp(const void*, const void*, int);
void      bzero(void*, int);
}
















//# 43 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/string.h" 2



//# 25 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/memory.h" 1

//# 49 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/memory.h"

//# 26 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/unistd.h" 1



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1
//# 12 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h"

//# 4 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/unistd.h" 2


extern "C" {


void _exit(int);




unsigned  alarm(unsigned);
int       brk(void*);
int       chdir(const char*);
int       chmod(const char*, int);
int       chown(const char*, int, int);
int       close(int);
char*     crypt(const char*, const char*);
int       dup(int);
int       dup2(int, int);
char*     encrypt(char*, int);
int       execl(const char*, const char *, ...);
int       execle(const char*, const char *, ...);
int       execlp(const char*, const char*, ...);
int       exect(const char*,  const char**,  char**);
int       execv(const char*,  const char**);
int       execve(const char*, const char**, char**);
int       execvp(const char*,  const char**);
int       fchown(int, int, int);
int       fork(void);
int       fsync(int);
int       ftruncate(int, unsigned long);
char*     getcwd(char*, int);
int       getdomainname(char*, int);
int       getdtablesize(void);

int       getgroups(int, int*);
int       geteuid(void);
int       getegid(void);
int       getgid(void);
long      gethostid(void);
int       gethostname(char*, int);
int       getpgrp(...);
int       getpid(void);
int       getppid(void);
char*     getlogin(void);
char*     getpass(const char*);
unsigned  getuid(void);
int       ioctl(int, int, void*);
int       isatty(int);
int       link(const char*, const char*);
int       mkstemp(char*);
char*     mktemp(char*);
int       nice(int);

void volatile pause(void);



int       pipe(int*);
int       readlink(const char*, char*, int);
int       rename(const char*, const char*);
int       rmdir(const char*);
void*     sbrk(int);
int       syscall(int, ...);
int       setgid(int);
int       sethostname(const char*, int);
int       setpgrp(...);
int       setregid(int, int);
int       setreuid(int, int);
int       setuid(int);
unsigned  sleep(unsigned);
void      swab(void*, void*, int);
int       symlink(const char*, const char*);
int       truncate(const char*, unsigned long);
char*     ttyname(int);
int       ttyslot(void);
// int       umask(int); /* commented out for now; wrong for SunOs4.1 */
int       unlink(const char*);
int       vfork(void);
int       vadvise(int);
int       vhangup(void);

long      lseek(int, long, int);
int       read(int, void*, size_t);
int       write(int, const void*, size_t);
int       access(const char*, int);

int       flock(int, int);


}




//# 27 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdio.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
 *  Please check the following before installing this file:
 *
 *  Make sure USG is #defined if you are on a USG system!
 *
 *  Check the value of _NFILE against the one in your /usr/include/stdio.h.
 *  (USG only)
 *
 *  Check whether your libc.a sprintf function returns
 *  an int (as do most) versus a char* (BSD), and (un)comment
 *  the corresponding SPRINTF_RETURNS_INT line.
 *
 *  Check the value of BUFSIZ against the one in your /usr/include/stdio.h.
 *
 *  Carefully check the fields and order of _iobuf declaration against
 *  the one in your /usr/include/stdio.h. Xenix-based systems
 *  may need some re-ordering of _iobuf. fields.
 *
 *  Note that some _IOXXX #defines may not be present in your
 *  /usr/include/stdio.h. This is ok, so long as the ones that
 *  are present in both are set to the same values.
 *
 *  Some of the prototypes refer to functions that may not be
 *  present in your libc.a. This is ok so long as you do not
 *  actually call such functions.
 *
 */



//#pragma interface







// Note:  The #define _stdio_h is at the end of this file,
// in case #include_next  finds an installed version of this
// same file -- we want it to continue until it finds the C version.

/*
   HAVE_VPRINTF should be set if vprintf is in libc.a
   HAVE_SETVBUF should be set if setvbuf is in libc.a
   HAVE_SETLINEBUF should be set if setlinebuf in libc.a

   The following are probably correct for the listed systems

*/





//# 85 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdio.h"



//# 158 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdio.h"


extern "C" {




























//# 1 "/usr/include/stdio.h" 1
/*	@(#)stdio.h	4.6	(ULTRIX)	3/1/91	*/
/************************************************************************
 *									*
 *			Copyright (c) 1985 by				*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   This software is  derived  from  software  received  from  the	*
 *   University    of   California,   Berkeley,   and   from   Bell	*
 *   Laboratories.  Use, duplication, or disclosure is  subject  to	*
 *   restrictions  under  license  agreements  with  University  of	*
 *   California and with AT&T.						*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/
/************************************************************************
 *			Modification History
 *
 *	Mitch Condylis 28-Feb-1991
 *	Changed _file member of FILE structure from char to short
 *	as part of work to increase max number of open file descriptors.
 *
 *	Mike Thomas, 7-Sep-1990
 * 016	Back out most of 015. Leave in explicit declarations.
 *
 * 	Mike Thomas, 21-Aug-1990
 * 015	DECwest ANSI - change _filbuf to __filbuf for ANSI compliance.
 *	Explicitly declare __filbuf under __STDC__.
 *	Likewise flsbuf.
 *
 * 014  Mike Thomas, 08-Jun-90
 *	Changed _POSIX_SOURCE reference back to POSIX.
 *
 * 013  Dan Smith, 23-Feb-90
 *      Added const to several prototypes. More namespace protection.
 *      Changed reference of POSIX to _POSIX_SOURCE.
 *
 *	Jon Reeves, 07-Dec-1989
 * 012	Namespace protection.
 *
 *	Jon Reeves, 09-Nov-1989
 * 011	Fix putc properly: could still sign-extend in some cases before.
 *
 *	Linda Wilson, 06-Oct-1989
 * 010  Declare sprintf as int for std conformance
 *
 *	Jon Reeves, 25-Aug-1989
 * 009	Fix putc[har] for 8-bit mode (unsigned int->char)
 *
 *	Jon Reeves, 18-Jul-1989
 * 008	Add getw, putw for X/Open conformance.
 *
 *	Jon Reeves, 31-May-1989
 * 007	ANSI conformance; clean up rogue comments.  sprintf is still
 *	wrong.
 *
 *	Lu Anne Van de Pas, 02-Jun-1986
 * 006  Added ending "/" to P_tmpdir string.
 *
 *	David L Ballenger, 22-Nov-1985
 * 005	Correct definition of sprintf() for System V environment.
 *
 *	David L Ballenger, 01-Aug-1985
 * 004	Add _IOAPPEND flag for files opened with "A" or "A+".
 *
 *	David L Ballenger, 26-Jun-1985
 * 003	Add changes so that FILE structures are allocated dynamically.
 *
 *	Larry Cohen, 23-April-1985
 *      - change NFILE from 20 to 64
 *
 *	David L Ballenger, 13-Mar-1985
 * 0001	Add System V definitions.
 ************************************************************************/

//# 1 "/usr/include/ansi_compat.h" 1
/*
 * 	@(#)ansi_compat.h	4.4	(ULTRIX)	10/8/90
 */

/************************************************************************
 *									*
 *			Copyright (c) 1990 by			        *
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

/*
 *   To avoid namespace pollution when using the ULTRIX header files under the
 * DEC ANSI compiler, all user-visible header files were modifed to reference
 * ANSI-style predefined macro name rather than their traditional names
 * (__ultrix vice ultrix).  Every file which accesses a predefined macro name
 * must include this file before any other files are included or the macros
 * are tested.
 *
 *   In strict ANSI mode, the appropriate ANSI-style macros are already
 * defined and the redefinitions in this file will not be seen.  When using
 * pcc, the traditional macro names are defined and this file will define
 * ANSI-style equivalents of the traditional names.  When using the DEC C
 * compiler, both the traditional and ANSI predefined macro names are
 * available so the definitions in this file are not made visible.
 *
 */


//# 116 "/usr/include/ansi_compat.h"

//# 89 "/usr/include/stdio.h" 2













			/* Note: spacing must match, too, to avoid warnings */




extern	struct	_iobuf {
	int	_cnt;
	char	*_ptr;
	char	*_base;
	int	_bufsiz;
	short	_flag;
	short	_file;
} _iob[3 ];
typedef	struct _iobuf	FILE;

typedef	long	fpos_t;

















/*	fseek() values	*/







/*
 *  prototype
 *
 */
extern int 	getc( FILE *__stream );
extern int	getchar( void );
extern int	putc( int __c, FILE *__stream);
extern int	putchar( int __c);
extern int	feof( FILE *__stream );
extern int	ferror( FILE *__stream );
extern int	fileno( FILE *__stream );
extern int	_filbuf( FILE *p);
extern int	_flsbuf( unsigned char x , FILE *p);
















typedef char *va_list;

/*
 *  prototypes
 *
 */
extern void	clearerr( FILE *__stream);
extern int	fclose( FILE *__stream );
extern FILE *	c_proto_fdopen ( int __filedes, char *__type );
extern int	fflush( FILE *__stream );
extern int	fgetc( FILE *__stream );
extern int	fgetpos( FILE *__stream, fpos_t *__pos );
extern char *	fgets( char *__s, int __n, FILE *__stream );
extern FILE *	c_proto_fopen ( const char *__filename, const char *__type );
extern int	c_proto_fprintf ( FILE *__stream, const char *__format, ... );
extern int	fputc( int __c, FILE *__stream );
extern int	c_proto_fputs ( const char *__s, FILE *__stream );
extern size_t	fread( void *__ptr, size_t __size,
			size_t __nitems, FILE *__stream );
extern FILE *	c_proto_freopen ( const char *__filename, const char *__type,
			FILE *__stream );
extern int	c_proto_fscanf ( FILE *__stream, const char *__format, ... );
extern int	fseek( FILE *__stream, long __offset, int __ptrname );
extern int	fsetpos( FILE *__stream, const fpos_t *__pos );
extern long	ftell( FILE *__stream );
extern size_t	c_proto_fwrite ( const void *__ptr, size_t __size,
			size_t __nitems, FILE *__stream );
extern char *	gets( char *__s );
extern void	c_proto_perror ( const char *__s );
extern FILE  *	c_proto_popen (const char *__command, const char *__type );
extern int	c_proto_printf ( const char *__format, ... );
extern int	c_proto_puts ( const char *__s );
extern int	remove( const char *__filename );
extern int	rename( const char *__from, const char *__to );
extern void	c_proto_rewind ( FILE *__stream );
extern int	c_proto_scanf ( const char *__format, ... );
extern void	c_proto_setbuf ( FILE *__stream, char *__buf );
extern int	c_proto_setvbuf ( FILE *__stream, char *__buf,
			int __type, size_t __size );
extern int	c_proto_sscanf ( const char *__s, const char *__format, ... );
extern FILE *	tmpfile( void );
extern char *	tmpnam( char *__s );
extern int	ungetc( int __c, FILE *__stream );
extern int	c_proto_vfprintf ( FILE *__stream, const char *__format, va_list __ap );
extern int	c_proto_vprintf ( const char *__format, va_list __ap );
extern int	c_proto_vsprintf ( char *__s, const char *__format, va_list __ap);


extern char *	c_proto_tempnam ( const char *__dir, const char *__pfx);
extern int	c_proto_putw ( int __w, FILE *__stream );
extern int	getw(FILE *__stream);
extern int	pclose( FILE *__stream );


//# 298 "/usr/include/stdio.h"



/* function prototype */
extern int	c_proto_sprintf ( char *__s, const char *__format, ... );



















//# 189 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdio.h" 2






























}





extern "C" {

//# 239 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stdio.h"

int    fclose(FILE*);
FILE*  fdopen(int, const char*);
int    fflush(FILE*);
int    fgetc(FILE*);
char*  fgets(char*, int, FILE *);
FILE*  fopen(const char*, const char*);
int    fprintf(FILE*, const char* ...);
int    fputc(int, FILE*);
int    fputs(const char*, FILE*);
size_t fread(void*, size_t, size_t, FILE*);



FILE*  freopen(const char*, const char*, FILE*);

int    fscanf(FILE*, const char* ...);
int    fseek(FILE*, long, int);
long   ftell(FILE *);
unsigned int fwrite(const void*, unsigned int, unsigned int, FILE*);
char*  gets(char*);
int    getw(FILE*);
int    pclose(FILE*);
void   perror(const char*);
FILE*  popen(const char*, const char*);
int    printf(const char* ...);
int    puts(const char*);
int    putw(int, FILE*);
int    rewind(FILE*);
int    scanf(const char* ...);
int    setbuf(FILE*, char*);
int    setbuffer(FILE*, char*, int);
int    setlinebuf(FILE*);
int    setvbuf(FILE*, char*, int, unsigned int);
int    sscanf(char*, const char* ...);
FILE*  tmpfile();
int    ungetc(int, FILE*);
int    vfprintf(FILE*, const char*, ...);

// Third arg to vprintf must be '...' for some machines, & doesn't
// hurt for others.

int    vprintf(const char*, ... );





char*  sprintf(char*, const char*, ...);
char*  vsprintf(char*, const char*, ...);


}


















//# 28 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/errno.h" 1


extern "C" {





//# 1 "/usr/include/errno.h" 1
/*	@(#)errno.h	2.4	(ULTRIX)	11/10/89	*/

/************************************************************************
 *									*
 *			Copyright (c) 1984, 1987 by			*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   This software is  derived  from  software  received  from  the	*
 *   University    of   California,   Berkeley,   and   from   Bell	*
 *   Laboratories.  Use, duplication, or disclosure is  subject  to	*
 *   restrictions  under  license  agreements  with  University  of	*
 *   California and with AT&T.						*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/
/*
 *
 *   Modification history:
 *
 * 10 Jul 89 -- jlr
 *	Added ENOSYS for POSIX
 *
 * 13 Jan 88 -- map
 *	Added ENOLCK for POSIX
 *
 * 4 Aug 86 -- chet
 *	Moved NFS error codes to match SUN's.
 *
 * 24 Feb 86 -- depp
 *	Added EALIGN error code
 *
 * 28-Mar-85 -- David L Ballenger
 *	Add mapping of System V error numbers from BRL package.
 *
 * 01 Mar 85 -- depp
 *	Added System V IPC error codes error codes.
 *
 */

/*
 * Error codes
 */


































/* math software */



/* non-blocking and interrupt i/o */



/* ipc/network software */

	/* argument errors */













	/* operational errors */













	/* */



/* should be rearranged */




/* quotas & mush */




/* NFS error codes */



/* IPC errors
 */



/* Alignment error of some type (i.e., cluster, page, block ...) */


/* System V mappings from BRL package
 */


/* POSIX errnos
 */



/*
 * DUP (Diagnostic/Utilities Protocol) related error numbers.
 */














/* External definition of errno from System V
 */

extern int errno;

//# 9 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/errno.h" 2




extern char*    sys_errlist[];
extern int      sys_nerr;
extern int      errno;
void      perror(const char*);
char*     strerr(int);


}


//# 29 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 2



//# 45 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine++.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/builtin.h" 1
// This may look like C code, but it is really -*- C++ -*-

/*
Copyright (C) 1988, 1992 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
  arithmetic, etc. functions on built in types
*/




//#pragma interface



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1
//# 12 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h"

//# 31 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/builtin.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988, 1992 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/


//# 31 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/std.h"

//# 32 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/builtin.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/math.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/




//#pragma interface











//# 64 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/math.h"

extern "C" {

double  acos(double);
double  acosh(double);
double  asin(double);
double  asinh(double);
double  atan(double);
double  atan2(double, double);
double  atanh(double);
double  cbrt(double);
double  ceil(double);
double  copysign(double,double);
double  cos(double);
double  cosh(double);
double  drem(double,double);
double  erf(double);
double  erfc(double);
double  exp(double);
double  expm1(double);
double  fabs(double);
double  finite(double);
double  floor(double);
double  frexp(double, int*);
double  gamma(double);
double  hypot(double,double);
double  infnan(int);

/* see below */
int     isinf(double);
int     isnan(double);

double  j0(double);
double  j1(double);
double  jn(int, double);
double  ldexp(double, int);
double  lgamma(double);
double  log(double);
double  log10(double);
double  log1p(double);
double  logb(double);
double  modf(double, double*);
double  pow(double, double);
double  rint(double);
double  scalb(double, int);
double  sin(double);
double  sinh(double);
double  sqrt(double);
double  tan(double);
double  tanh(double);
double  y0(double);
double  y1(double);
double  yn(int, double);

double aint(double);
double anint(double);
int irint(double);
int nint(double);
}



/* libg++ doesn't use this since it is not available on some systems */

/* the following ifdef is just for compiling OOPS */


struct libm_exception
{
  int type;
  char* name;
  double arg1, arg2, retval;
};








extern "C" int matherr(libm_exception*);



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/values.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/



























//# 150 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/values.h"

























//# 149 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/math.h" 2


/* On some systems, HUGE ought to be MAXFLOAT or IEEE infinity */






/* sequents don't supply these. The following should suffice */









/* These seem to be sun & sysV names of these constants */

















































//# 33 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/builtin.h" 2


typedef void (*one_arg_error_handler_t)(const char*);
typedef void (*two_arg_error_handler_t)(const char*, const char*);

long         gcd(long, long);
long         lg(unsigned long);
double       pow(double, long);
long         pow(long, long);

double       start_timer();
double       return_elapsed_time(double last_time = 0.0);

char*        itoa(long x, int base = 10, int width = 0);
char*        itoa(unsigned long x, int base = 10, int width = 0);

char*        itoa(long long x, int base = 10, int width = 0);
char*        itoa(unsigned long long x, int base = 10, int width = 0);

char*        dtoa(double x, char cvt = 'g', int width = 0, int prec = 6);

char*        hex(long x, int width);
char*        hex(unsigned long x, int width);
char*        hex(int x, int width);
char*        hex(short x, int width);
char*        hex(unsigned int x, int width);
char*        hex(unsigned short x, int width);

char*        oct(long x, int width);
char*        oct(unsigned long x, int width);
char*        oct(int x, int width);
char*        oct(short x, int width);
char*        oct(unsigned int x, int width) ;
char*        oct(unsigned short x, int width);

char*        dec(long x, int width);
char*        dec(unsigned long x, int width);
char*        dec(int x, int width);
char*        dec(short x, int width);
char*        dec(unsigned int x, int width) ;
char*        dec(unsigned short x, int width);

char*        form(const char* fmt ...);
char*        chr(char ch, int width = 0);
char*        str(const char* s, int width = 0);

unsigned int hashpjw(const char*);
unsigned int multiplicativehash(int);
unsigned int foldhash(double);

extern void default_one_arg_error_handler(const char*);
extern void default_two_arg_error_handler(const char*, const char*);

extern two_arg_error_handler_t lib_error_handler;

extern two_arg_error_handler_t
       set_lib_error_handler(two_arg_error_handler_t f);


double abs(double arg);
float abs(float arg);
short abs(short arg);
long abs(long arg);
int sign(long arg);
int sign(double arg);
long sqr(long arg);
double sqr(double arg);
int even(long arg);
int odd(long arg);
long lcm(long x, long y);
void setbit(long& x, long b);
void clearbit(long& x, long b);
int testbit(long x, long b);

signed char min(signed char a, signed char b);
unsigned char min(unsigned char a, unsigned char b);

signed short min(signed short a, signed short b);
unsigned short min(unsigned short a, unsigned short b);

signed int min(signed int a, signed int b);
unsigned int min(unsigned int a, unsigned int b);

signed long min(signed long a, signed long b);
unsigned long min(unsigned long a, unsigned long b);

float min(float a, float b);

double min(double a, double b);

signed char max(signed char a, signed char b);
unsigned char max(unsigned char a, unsigned char b);

signed short max(signed short a, signed short b);
unsigned short max(unsigned short a, unsigned short b);

signed int max(signed int a, signed int b);
unsigned int max(unsigned int a, unsigned int b);

signed long max(signed long a, signed long b);
unsigned long max(unsigned long a, unsigned long b);

float max(float a, float b);

double max(double a, double b);




inline double abs(double arg)
{
  return (arg < 0.0)? -arg : arg;
}

inline float abs(float arg)
{
  return (arg < 0.0)? -arg : arg;
}

inline short abs(short arg)
{
  return (arg < 0)? -arg : arg;
}

inline long abs(long arg)
{
  return (arg < 0)? -arg : arg;
}

inline int sign(long arg)
{
  return (arg == 0) ? 0 : ( (arg > 0) ? 1 : -1 );
}

inline int sign(double arg)
{
  return (arg == 0.0) ? 0 : ( (arg > 0.0) ? 1 : -1 );
}

inline long sqr(long arg)
{
  return arg * arg;
}

inline double sqr(double arg)
{
  return arg * arg;
}

inline int even(long arg)
{
  return !(arg & 1);
}

inline int odd(long arg)
{
  return (arg & 1);
}

inline long lcm(long x, long y)
{
  return x / gcd(x, y) * y;
}

inline void setbit(long& x, long b)
{
  x |= (1 << b);
}

inline void clearbit(long& x, long b)
{
  x &= ~(1 << b);
}

inline int testbit(long x, long b)
{
  return ((x & (1 << b)) != 0);
}




//# 46 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine++.h" 2


//# 65 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine++.h"


// Take the nice stuff that the regular sumachine.h provides
extern "C" {
//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h" 1



/*
 *    Define a ``Standard Unix Machine''
 *
 *    By whatever means are available in /lib/cpp, figure out
 *    what type of operating system, maker, machine architecture
 *    and architecture instance we are on.
 *
 *    OPSYS_TYPE	- the type of operating system (e.g. vms)
 *    OPSYS_SUBTYPE	- the subtype of operating system (e.g. bsd, sys5, etc)
 *    OPSYS_MFR		- the manufacturer (e.g. Hewlett-Packard)
 *    OPSYS_VERS	- the operating system version (e.g. sun 4.0)
 *
 *    CPU_TYPE		- the architecture (e.g. 68K)
 *    CPU_MODEL		- the architecture instance (e.g. 68010)
 *
 *
 *    This file should be #included as the 1st line of each and every .c file
 *
 *    It ensures that the environment which those src files expect exists
 *    either by fiat or by faking it where the host operating system is not
 *    up to snuff.
 */

/*
 *    The OPSYS specifics
 *
 *    Parameterized with the best known /lib/cpp predefines
 */









/*
 *    Note that everyone defines the symbol unix except IBM's AIX 3.1
 *    which doesn't define unix but DOES define the symbol _AIX
 */





















/*
 *    WATCHOUT - the newer gcc doesn't define __GNU__
 *    So we may have to define __GNU__ ourselves
 *
 *    __GNU__ is the old way
 *    __GNUC__ is the new way
 *
 *    Use gcc -v somefile.c to determine what is being passed.
 */

/*
 *    GNU doesn't know about any opsys-specific
 *    cpp tokens, so we must figure them out for it.
 *    GNU does however know about architecture-tokens
 */
//# 93 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"




/* then if  defined(vax) || defined(mips) */
/* then if >= ultrix 4.2 => defined(__vax) || defined(__mips) */


/*
 *    gcc 1.39 uses the words mips and __mips__ while Ultrix 4.2 has
 *    been coded in terms of __mips (only!) - so we have to hack it.
 *
 *    See  from Ultrix 4.2
 *
 *    Unfortunately also,  is turned on only if __STDC__
 *    is undefined.  gcc defines __STDC__ and you can't undef it.  So
 *    We are forced here (as with math.h) to copy  and
 *    strip off the #if !defined(__STDC__).
 *
 *    Further unfortunateness occurs in that the only way we have of
 *    discovering that we're in Ultrix 4.2 is to #include
 *    and check for the vector unit support #defined there.  However
 *    we need the  workarounds to be defined first in
 *    order to get the full effect of .
 *
 *    So we have to do all this stuff under any and all Ultrix versions.
 *
 *    We also have to work around the fact that the GNU (gcc 1.95.0 and later)
 *    compilers may be defining this stuff already
 */




































































/* end of  fakeout */


/*
 *   The r2000 and r3000 are indistinguishable (so far)
 */


//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/sys/types.h" 1


extern "C"
{





//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1
//# 12 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h"

//# 10 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/sys/types.h" 2





//# 1 "/usr/include/sys/types.h" 1
/* @(#)types.h	4.3	(ULTRIX)	2/28/91 */

/************************************************************************
 *									*
 *			Copyright (c) 1984 - 1989 by			*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   This software is  derived  from  software  received  from  the	*
 *   University    of   California,   Berkeley,   and   from   Bell	*
 *   Laboratories.  Use, duplication, or disclosure is  subject  to	*
 *   restrictions  under  license  agreements  with  University  of	*
 *   California and with AT&T.						*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

/* ------------------------------------------------------------------------
 * Modification History: /sys/h/types.h
 *
 * 20-Dec-1989  Larry Scott
 *	added #ifdef's for compliance
 *
 * 16-Jun-1989	Jon Reeves
 *	size_t must be unsigned, per ANSI and X/Open
 *
 * 05-Jun-1989	Jon Reeves
 *	Change latch name for size_t; it's in lots of headers.
 *
 * 12-May-1989	Todd M. Katz		TMK0001
 *	Added volatile type definitions: vu_long, vu_short, vu_char,
 *	v_long, v_short, and v_char.
 *
 * 08-May-1989 -- Ken Lesniak
 *	Conditionalize items also defined in time.h.
 *
 *  1-Feb-89 -- jmartin
 *	typedef s_char
 *
 * 15-Jan-88	lp
 *	Merge of final 43BSD changes.
 *
 * 31-August-1987 -- Mark Parenti
 *	Add definitions needed for POSIX compliance
 *
 * 27-April-1987 -- Larry Cohen
 *	Modify the typedef "fd_set" to accomodate 64 file descriptors.
 *
 * 	David L Ballenger, 8-Mar-1985
 * 0002	Add types for System V compatibility.
 *
 * 23 Oct 84 -- jrs
 *	Add ifdef so we can be nested without problem
 *	Derived from 4.2BSD, labeled:
 *		types.h 6.2	84/06/09
 *
 * -----------------------------------------------------------------------
 */







//# 1 "/usr/include/ansi_compat.h" 1
/*
 * 	@(#)ansi_compat.h	4.4	(ULTRIX)	10/8/90
 */

/************************************************************************
 *									*
 *			Copyright (c) 1990 by			        *
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

/*
 *   To avoid namespace pollution when using the ULTRIX header files under the
 * DEC ANSI compiler, all user-visible header files were modifed to reference
 * ANSI-style predefined macro name rather than their traditional names
 * (__ultrix vice ultrix).  Every file which accesses a predefined macro name
 * must include this file before any other files are included or the macros
 * are tested.
 *
 *   In strict ANSI mode, the appropriate ANSI-style macros are already
 * defined and the redefinitions in this file will not be seen.  When using
 * pcc, the traditional macro names are defined and this file will define
 * ANSI-style equivalents of the traditional names.  When using the DEC C
 * compiler, both the traditional and ANSI predefined macro names are
 * available so the definitions in this file are not made visible.
 *
 */


//# 116 "/usr/include/ansi_compat.h"

//# 79 "/usr/include/sys/types.h" 2




/*
 * Basic system types and major/minor device constructing/busting macros.
 */

/* major part of a device */


/* minor part of a device */


/* make a device number */


typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned int	uint;		/* sys V compatibility */
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		/* sys III compat */

typedef	volatile char		v_char;
typedef	volatile short		v_short;
typedef	volatile long		v_long;
typedef	volatile unsigned char	vu_char;
typedef	volatile unsigned short	vu_short;
typedef	volatile unsigned long	vu_long;

typedef

	signed

		char	s_char;









typedef	struct	_physadr { int r[1]; } *physadr;
/*
 * WARNING:
 * this must match the definition of kernel jmpbuf's in machine/pcb.h
 */
typedef	struct	label_t	{
	int	val[12];
} label_t;


typedef	struct	_quad { long val[2]; } quad;
typedef	long	daddr_t;
typedef	char *	caddr_t;
typedef u_long	gno_t;
typedef short	cnt_t;			/* sys V compatibility */
typedef	long	swblk_t;
typedef long	paddr_t;		/* sys V compatibility */
typedef	long	audit_ID_t;


typedef	short	dev_t;
typedef short	gid_t;			/* POSIX compliance    */
typedef	unsigned long	ino_t;
typedef unsigned short	mode_t;		/* POSIX compliance    */
typedef short	nlink_t;		/* POSIX compliance    */
typedef	int	off_t;


typedef int	pid_t;			/* POSIX compliance    */

typedef short	uid_t;			/* POSIX compliance    */


typedef int	time_t;









typedef int	clock_t;			/* POSIX compliance */

typedef long	key_t;			/* sys V compatibility */




/*
 * The maximum number of file descriptors is now a configurable option
 * (max_nofile variable in /sys/conf/{mips|vax}/param.c).
 * The getdtablesize(2) system call should be used to obtain the
 * current limit. The value returned by getdtablesize() must be greater
 * than 64, and less than or equal to MAX_NOFILE in types.h . The
 * MAX_NOFILE define is needed for backward compatability with broken
 * programs that need a static sized array for selecting. These programs
 * should be modified to use the getdtablesize() interface for sizing.
 */


/*
 * Select uses bit masks of file descriptors in longs.
 * These macros manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here
 * should be >= NOFILE (param.h).
 */




/* How many things we'll allow select to use. 0 if unlimited */

typedef long	fd_mask;






typedef	struct fd_set {
	fd_mask	fds_bits[(((4096	 )+(( (sizeof(fd_mask) * 8		)	)-1))/( (sizeof(fd_mask) * 8		)	)) ];
} fd_set;








//# 15 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/sys/types.h" 2






}




//# 199 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/signal.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1989 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/



extern "C" {




//# 1 "/usr/include/signal.h" 1
/*	@(#)signal.h	2.8	(ULTRIX)	11/9/89	*/

/************************************************************************
 *									*
 *			Copyright (c) 1987-1989 by			*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   This software is  derived  from  software  received  from  the	*
 *   University    of   California,   Berkeley,   and   from   Bell	*
 *   Laboratories.  Use, duplication, or disclosure is  subject  to	*
 *   restrictions  under  license  agreements  with  University  of	*
 *   California and with AT&T.						*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/
/************************************************************************
 *			Modification History				*
 *									*
 *	Debby Haeck 11/14/90						*
 *		added new Vector Arithmetic Exception handling codes	*
 *	Debby Haeck 9/4/90						*
 *		added new u_code for vector support			*
 *		ILL_VECOP_FAULT, ILL_VECINST_FAULT, TERM_VECT_HARD and	*
 *		TERM_VECT_TOOMANY					*
 *	Tak Yin Wong 3/390                                              *
 *		Add ifdef's for POSIX and XOPEN                         *
 *	Linda Wilson 9/12/89                                            *
 *		typedef sigset_t for X/OPEN			        *
 *	Linda Wilson 9/12/89                                            *
 *      	ifdef out sigmask for POSIX                             *
 *	Jon Reeves 7/14/89						*
 *		Add X/Open mandated function declarations.		*
 *	Jon Reeves 5/16/89						*
 *		Add new BRK_STACKOVERFLOW def from MIPS 2.0 cmplrs	*
 *	Jon Reeves 5/12/89						*
 *		Add raise() and sig_atomic_t for ANSI			*
 *	Mark Parenti 2/06/88						*
 *		Change SA_CLDSTOP to SA_NOCLDSTOP per POSIX change	*
 *	Fred Glover 1/12/88						*
 *		Add SIGLOST - server crash Sys-V lock notification      *
 *									*
 *	Larry Cohen 10/1/85						*
 *		Add SIGWINCH - window change signal			*
 *									*
 *	Greg Depp  25 Jun 85						*
 *	Moved SIGUSR1 and SIGUSR2 to 30 and 31 to conform with Berkeley *
 *									*
 *	David L Ballenger, 28-Mar-1985					*
 * 0001 Add definitions for System V compatibility			*
 *									*
 ************************************************************************/








//# 1 "/usr/include/ansi_compat.h" 1
/*
 * 	@(#)ansi_compat.h	4.4	(ULTRIX)	10/8/90
 */

/************************************************************************
 *									*
 *			Copyright (c) 1990 by			        *
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

/*
 *   To avoid namespace pollution when using the ULTRIX header files under the
 * DEC ANSI compiler, all user-visible header files were modifed to reference
 * ANSI-style predefined macro name rather than their traditional names
 * (__ultrix vice ultrix).  Every file which accesses a predefined macro name
 * must include this file before any other files are included or the macros
 * are tested.
 *
 *   In strict ANSI mode, the appropriate ANSI-style macros are already
 * defined and the redefinitions in this file will not be seen.  When using
 * pcc, the traditional macro names are defined and this file will define
 * ANSI-style equivalents of the traditional names.  When using the DEC C
 * compiler, both the traditional and ANSI predefined macro names are
 * available so the definitions in this file are not made visible.
 *
 */


//# 116 "/usr/include/ansi_compat.h"

//# 74 "/usr/include/signal.h" 2


















					/* absent or non-vector capable sys */
/* CHME, CHMS, CHMU are not yet given back to users reasonably */



















/* the following are used to for Vector Arithmetic Exception handling */







































/* Add System V signal definitions (DLB001) */







/*
 * Codes for the mips break instruction.
 */
















/* Accesses to sig_atomic_t are atomic, even with async interrupts.
   Not an issue for us, but ANSI requires the definition. */
typedef long	sig_atomic_t;
typedef int sigset_t;	/* type used for sigsetops() functions  */


/*
 * Signal vector "template" used in sigvec call.
 */
struct	sigvec {
	void	 (*sv_handler)();	/* signal handler */
	sigset_t sv_mask;		/* signal mask to apply */
	int	sv_flags;		/* see signal options below */
};


/*
 * The following structure must be exactly the same as the above structure
 * with the names changed for POSIX compliance.
 */
struct	sigaction {
	void	 (*sa_handler)();	/* signal handler */
	sigset_t sa_mask;		/* signal mask to apply */
	int	 sa_flags;		/* see signal options below */
};














/* Defines for sigprocmask() call. POSIX.
 */






/*
 * Structure used in sigstack call.
 */
struct	sigstack {
	char	*ss_sp;			/* signal stack pointer */
	int	ss_onstack;		/* current status */
};

/*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to properly restore state if
 * a non-standard exit is performed.
 *
//#ifdef __vax
 *	XXX - sigcontext needs updating per 4.3BSD - rr
 *
//#endif __vax
//#ifdef __mips
 * WARNING: THE sigcontext MUST BE KEPT CONSISTENT WITH /usr/include/setjmp.h
 * AND THE LIBC ROUTINES setjmp() AND longjmp()
//#endif __mips
 */
struct	sigcontext {

	/*
	 * BEGIN REGION THAT MUST CORRESPOND WITH setjmp.h
	 * BEGIN REGION THAT MUST CORRESPOND WITH A jmp_buf
	 */

	int	sc_onstack;		/* sigstack state to restore */
	int	sc_mask;		/* signal mask to restore */






	int	sc_pc;			/* pc at time of signal */
	/*
	 * General purpose registers
	 */
	int	sc_regs[32];	/* processor regs 0 to 31 */
	int	sc_mdlo;	/* mul/div low */
	int	sc_mdhi;	/* mul/div high */
	/*
	 * Floating point coprocessor state
	 */
	int	sc_ownedfp;	/* fp has been used */
	int	sc_fpregs[32];	/* fp regs 0 to 31 */
	int	sc_fpc_csr;	/* floating point control and status reg */
	int	sc_fpc_eir;	/* floating point exception instruction reg */
	/*
	 * END OF REGION THAT MUST AGREE WITH setjmp.h
	 * END OF jmp_buf REGION
	 */
	/*
	 * System coprocessor registers at time of signal
	 */
	int	sc_cause;	/* cp0 cause register */
	int	sc_badvaddr;	/* cp0 bad virtual address */
	int	sc_badpaddr;	/* cpu bd bad physical address */

};























/*
 *  prototypes
 *
 */
extern void	(*signal(int __sig, void(*__func)(int)))(int);
int	raise( int __sig );
int 	kill( pid_t __pid, int __sig );
int 	sigemptyset( sigset_t *__set );
int 	sigfillset( sigset_t *__set );
int 	sigaddset( sigset_t *__set, int __signo );
int	sigdelset( sigset_t *__set, int __signo );
int	sigismember( const sigset_t *__set, int __signo );
int 	sigaction( int __sig, const struct sigaction *__act,
		struct sigaction *__oact );
int 	sigprocmask( int __how, const sigset_t *__set, sigset_t *__oset );
int	sigpending( sigset_t *__set );
int	sigsuspend( const sigset_t *__sigmask );











/*
 * Macro for converting signal number to a mask suitable for
 * sigblock().
 */






//# 26 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/signal.h" 2




// The Interviews folks call this SignalHandler. Might as well conform.
// Beware: some systems think that SignalHandler returns int.
typedef void (*SignalHandler) (int);

extern SignalHandler signal(int sig, SignalHandler action);
extern SignalHandler sigset(int sig, SignalHandler action);
extern SignalHandler ssignal(int sig, SignalHandler action);
extern int           gsignal (int sig);
extern int           kill (int pid, int sig);
extern int           killpg(int, int);
extern int           siginterrupt(int, int);
extern void          psignal(unsigned, char*);


extern int           sigsetmask(int mask);
extern int           sigblock(int mask);
extern int           sigpause(int mask);
extern int           sigvec(int sig, struct sigvec* v, struct sigvec* prev);


// The Interviews version also has these ...













}



//# 200 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h" 2


/*
 *    Note that in Ultrix 4.2  renames all of the
 *    previously-predefined cpp names to __names.  Also there is
 *    new support for vector processors - TERM_VECT_HARD etc.
 */

//# 244 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"


//# 263 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"

//# 283 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"

/* { */
//# 298 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"

//# 319 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"

//# 328 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"

//# 345 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"


























//# 387 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"







/*
 *    THE CPU specifics
 *
 *    Parameterized with the best known /lib/cpp predefines
 *    Set up for a middle-of-the-road machine.
 */

/*
 *    DECstation	Processor
 *       2100		  R2000 (slower clock)
 *       3100		  R2000
 *       5000		  R3000
 *       5400		  R3000
 *       ????		  R6000
 */
/*
 *    We might be on the R3000 machine, but there is really
 *    no way of distinguishing that from the src-code level.
 *    The GNU people define some stuff, but its usually wrong.
 */

















//# 453 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"









//# 471 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"





























//# 512 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"

/* will we be porting to this? */
//# 522 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"


//# 537 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"


/*
 *    The following MUST be literals else /lib/cpp cannot do
 *    == operations on them.  Remember cpp does strcmps, not MATH
 *
 *    OPSYS_TYPE
 *
 *    #if OPSYS_TYPE == MD_opsystype
 *    #endif
 */






/*
 *    OPSYS_SUBTYPE
 *
 *    #if OPSYS_SUBTYPE == MD_opsyssubtype
 *    #endif
 */











/*
 *    OPSYS_MFR
 *
 *    #if OPSYS_MFR == MD_opsysmfr
 *    #endif
 */















/*
 *    OPSYS_VERS
 *
 *    #if OPSYS_VERS == MD_opsysvers
 *    #endif
 *    #if OPSYS_MFR == MD_opsysmfr && OPSYS_VERS <= MD_opsysvers
 *    #endif
 */




























































































































/*
 *    CPU_TYPE
 *
 *    #if CPU_TYPE == MD_cputype
 *    #endif
 */




















/*
 *    CPU_MODEL
 *
 *    #if CPU_MODEL == MD_modelname
 *    #endif
 */






























































/* don't add this stuff in if the query program display.sh is being run */

//# 1754 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine.h"





//# 69 "/sandbox/wbaker/wbaker0/source/mips/include/sumachine++.h" 2

}

// Always turn this stuff on b/c otherwise nothing works





//# 1 "GctNameRef.List.cc" 2

//# 1 "../../../../mips/include/Gct/genclasses.h" 1
// -*- C++ -*-



//
// genclasses - The GNU Container Classes
//
// Expects to be included by Gct.h or where appropriate
// especially before any genclass class headerfile is included.
//
// Wendell Baker, Berkeley CAD Group, 1991 (wbaker@ic.Berkeley.EDU)
//


// Make sure that we get all of the inline functions too but prevent duplicate
// definition warnings.










//
// Turn off the definitions of these hash functions
//
//     unsigned hash(GctAstRef&)
//
// So that the following will match and be used instead:
//
//     unsigned hash(GctRef&)
//
// The idea is to not allow the various header files to
// create (say) ``unsigned hash(GctAstRef&)'' but to use
// ``hash(x)'' for the hash function anyway.  We just want
// the hash() function used to be ``unsigned hash(GctRef&)''
//


//# 1 "../../../../mips/include/Gct/Reference.h" 1
// -*- C++ -*-



//
// Classes:
//     GctRef
//     GctReferenceObject
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "../../../../mips/include/Gct/Reference/GctReferenceCount.h" 1
// -*- C++ -*-



//
// GctReferenceCount class
//
// Expects to be included by Gct/Reference.h
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface





//# 25 "../../../../mips/include/Gct/Reference/GctReferenceCount.h" 2


//# 1 "../../../../mips/include/Gct/GctErrorHandler.h" 1
// -*- C++ -*-



//
// GctErrorHandler class
//
// Expects to be included by Gct.h
//
// Wendell Baker, Berkeley CAD Group, 1991 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/bool.h" 1







//# 25 "../../../../mips/include/Gct/GctErrorHandler.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/String.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/




//#pragma interface



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stream.h" 1



// Compatibility with old library.


//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.



//#pragma interface




//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/streambuf.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.




//#pragma interface


/* KLUDGES!! */
//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h" 1
//# 12 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stddef.h"

//# 25 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/streambuf.h" 2
















class ostream; class streambuf;

typedef long streamoff, streampos;

struct _ios_fields { // The data members of an ios.
    streambuf *_strbuf;
    ostream* _tie;
    long _width;
    unsigned long _flags;
    char _fill;
    unsigned char _state;
    unsigned short _precision;
};


enum state_value { _good = 0, _eof = 1,  _fail = 2, _bad  = 4 };


class ios : public _ios_fields {
  public:
    enum io_state { goodbit=0, eofbit=1, failbit=2, badbit=4 };
    enum open_mode {
	in=1,
	out=2,
	ate=4,
	app=8,
	trunc=16,
	nocreate=32,
	noreplace=64 };
    enum seek_dir { beg, cur, end};
    enum { skipws=01, left=02, right=04, internal=010,
	   dec=020, oct=040, hex=0100,
	   showbase=0200, showpoint=0400, uppercase=01000, showpos=02000,
	   scientific=04000, fixed=0100000, unitbuf=020000, stdio=040000,
	   dont_close=0x80000000 //Don't close streambuf when destroying stream
	   };

    ostream* tie() { return _tie; }
    ostream* tie(ostream* val) { ostream* save=_tie; _tie=val; return save; }

    // Methods to change the format state.
    char fill() { return _fill; }
    char fill(char newf) { char oldf = _fill; _fill = newf; return oldf; }
    unsigned long flags() { return _flags; }
    unsigned long flags(unsigned long new_val) {
	unsigned long old_val = _flags; _flags = new_val; return old_val; }
    unsigned short precision() { return _precision; }
    unsigned short precision(int newp) {
	unsigned short oldp = _precision; _precision = (unsigned short)newp;
	return oldp; }
    unsigned long setf(unsigned long val) {
	unsigned long oldbits = _flags;
	_flags |= val; return oldbits; }
    unsigned long setf(unsigned long val, unsigned long mask) {
	unsigned long oldbits = _flags;
	_flags = (_flags & ~mask) | (val & mask); return oldbits; }
    unsigned long unsetf(unsigned long mask) {
	unsigned long oldbits = _flags & mask;
	_flags &= ~mask; return oldbits; }
    long width() { return _width; }
    long width(long val) { long save = _width; _width = val; return save; }

    static const unsigned long basefield;
    static const unsigned long adjustfield;
    static const unsigned long floatfield;

    streambuf* rdbuf() { return _strbuf; }
    void clear(int state = 0) { _state = state; }
    int good() { return _state == 0; }
    int eof() { return _state & ios::eofbit; }
    int fail() { return _state & (ios::badbit|ios::failbit); }
    int bad() { return _state & ios::badbit; }
    int rdstate() { return _state; }
    void set(int flag) { _state |= flag; }
    operator void*() { return fail() ? (void*)0 : (void*)this; }
    int operator!() { return fail(); }


    void unset(state_value flag) { _state &= ~flag; }
    void close();
    int is_open();
    int readable();
    int writable();


  protected:
    ios(streambuf*sb) { _strbuf=sb; _state=0; _width=0; _fill=' ';
			_flags=ios::skipws; _precision=6; }
};




typedef ios::seek_dir _seek_dir;


// Magic numbers and bits for the _flags field.
// The magic numbers use the high-order bits of _flags;
// the remaining bits are abailable for variable flags.
// Note: The magic numbers must all be negative if stdio
// emulation is desired.















struct __streambuf {
    // NOTE: If this is changed, also change __FILE in stdio/stdio.h!
    int _flags;		/* High-order word is _IO_MAGIC; rest is flags. */
    char* _gptr;	/* Current get pointer */
    char* _egptr;	/* End of get area. */
    char* _eback;	/* Start of putback+get area. */
    char* _pbase;	/* Start of put area. */
    char* _pptr;	/* Current put pointer. */
    char* _epptr;	/* End of put area. */
    char* _base;	/* Start of reserve area. */
    char* _ebuf;	/* End of reserve area. */
    struct streambuf *_chain;
};

struct streambuf : private __streambuf {
    friend class ios;
    friend class istream;
    friend class ostream;
  protected:
    static streambuf* _list_all; /* List of open streambufs. */
    streambuf*& xchain() { return _chain; }
    void _un_link();
    void _link_in();
    char* gptr() const { return _gptr; }
    char* pptr() const { return _pptr; }
    char* egptr() const { return _egptr; }
    char* epptr() const { return _epptr; }
    char* pbase() const { return _pbase; }
    char* eback() const { return _eback; }
    char* ebuf() const { return _ebuf; }
    char* base() const { return _base; }
    void xput_char(char c) { *_pptr++ = c; }
    int xflags() { return _flags; }
    int xflags(int f) { int fl = _flags; _flags = f; return fl; }
    void xsetflags(int f) { _flags |= f; }
    void gbump(int n) { _gptr += n; }
    void pbump(int n) { _pptr += n; }
    void setb(char* b, char* eb, int a=0);
    void setp(char* p, char* ep) { _pbase=_pptr=p; _epptr=ep; }
    void setg(char* eb, char* g, char *eg) { _eback=eb; _gptr=g; _egptr=eg; }
  public:
    static int flush_all();
    static void flush_all_linebuffered(); // Flush all line buffered files.
    virtual int underflow(); // Leave public for now
    virtual int overflow(int c = (-1) ); // Leave public for now
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streampos seekpos(streampos pos, int mode = ios::in|ios::out);
    int sputbackc(char c);
    int sungetc();
    streambuf();
    virtual ~streambuf();
    int unbuffered() { return _flags & 2  ? 1 : 0; }
    int linebuffered() { return _flags & 0x4000  ? 1 : 0; }
    void unbuffered(int i)
	{ if (i) _flags |= 2 ; else _flags &= ~2 ; }
    void linebuffered(int i)
	{ if (i) _flags |= 0x4000 ; else _flags &= ~0x4000 ; }
    int allocate() {
	if (base() || unbuffered()) return 0;
	else return doallocate(); }
    virtual int sync();
    virtual int pbackfail(int c);
    virtual int ungetfail();
    virtual streambuf* setbuf(char* p, int len);
    int in_avail() { return _egptr - _gptr; }
    int out_waiting() { return _pptr - _pbase; }
    virtual int sputn(const char* s, int n);
    virtual int sgetn(char* s, int n);
    long sgetline(char* buf, size_t n, char delim, int putback_delim);
    int sbumpc() {
	if (_gptr >= _egptr && underflow() == (-1) ) return (-1) ;
	else return *(unsigned char*)_gptr++; }
    int sgetc() {
	if (_gptr >= _egptr && underflow() == (-1) ) return (-1) ;
	else return *(unsigned char*)_gptr; }
    int snextc() {
	if (++_gptr >= _egptr && underflow() == (-1) ) return (-1) ;
	else return *(unsigned char*)_gptr; }
    int sputc(int c) {
	if (_pptr >= _epptr) return overflow(c);
	return *_pptr++ = c, (unsigned char)c; }
    int vscan(char const *fmt0, char*  ap);
    int vform(char const *fmt0, char*  ap);
};

struct __file_fields {
    char _fake;
    char _shortbuf[1];
    short _fileno;
    int _blksize;
    char* _save_gptr;
    char* _save_egptr;
    long  _offset;
};

class filebuf : public streambuf {
    struct __file_fields _fb;
    void init();
  public:
    filebuf();
    filebuf(int fd);
    filebuf(int fd, char* p, int len);
    ~filebuf();
    filebuf* attach(int fd);
    filebuf* open(const char *filename, const char *mode);
    filebuf* open(const char *filename, int mode, int prot = 0664);
    virtual int underflow();
    virtual int overflow(int c = (-1) );
    int is_open() { return _fb._fileno >= 0; }
    int fd() { return is_open() ? _fb._fileno : (-1) ; }
    filebuf* close();
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    int sputn(const char* s, int n);
    int sgetn(char* s, int n);
  protected: // See documentation in filebuf.C.
    virtual int pbackfail(int c);
    virtual int sync();
    int is_reading() { return eback() != egptr(); }
    char* cur_ptr() { return is_reading() ?  gptr() : pptr(); }
    /* System's idea of pointer */
    char* file_ptr() { return _fb._save_gptr ? _fb._save_egptr : egptr(); }
    int do_flush();
    // Low-level operations (Usually invoke system calls.)
    virtual int sys_read(char* buf, size_t size);
    virtual long  sys_seek(long , _seek_dir);
    virtual long sys_write(const void*, long);
    virtual int sys_stat(void*); // Actually, a (struct stat*)
    virtual int sys_close();
};


inline int ios::readable() { return rdbuf()->_flags & 4 ; }
inline int ios::writable() { return rdbuf()->_flags & 8 ; }
inline int ios::is_open() {return rdbuf()->_flags & 4 +8 ;}




//# 25 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 2


class istream; class ostream;
typedef istream& (*__imanip)(istream&);
typedef ostream& (*__omanip)(ostream&);

extern istream& ws(istream& ins);
extern ostream& flush(ostream& outs);
extern ostream& endl(ostream& outs);
extern ostream& ends(ostream& outs);

class ostream : public ios
{
    void do_osfx();
  public:
    ostream();
    ostream(streambuf* sb, ostream* tied=0 );
    ~ostream();

    int opfx() { if (!good()) return 0; if (_tie) _tie->flush(); return 1; }
    void osfx() { if (flags() & (ios::unitbuf|ios::stdio))
		      do_osfx(); }
    streambuf* ostreambuf() const { return _strbuf; }
    ostream& flush();
    ostream& put(char c);
    ostream& write(const char *s, int n);
    ostream& write(const unsigned char *s, int n) { return write((char*)s, n);}
    ostream& write(const void *s, int n) { return write((char*)s, n);}
    ostream& seekp(streampos);
    ostream& seekp(streamoff, _seek_dir);
    streampos tellp();
    ostream& form(const char *format ...);
    ostream& vform(const char *format, char*  args);
};

ostream& operator<<(ostream&, char c);
ostream& operator<<(ostream& os, unsigned char c) { return os << (char)c; }
//ostream& operator<<(ostream &os, signed char c) { return os << (char)c; }
extern ostream& operator<<(ostream&, const char *s);
inline ostream& operator<<(ostream& os, const unsigned char *s)
{ return os << (const char*)s; }
//inline ostream& operator<<(ostream& os, const signed char *s)
//{ return os << (const char*)s; }
ostream& operator<<(ostream&, void *p);
ostream& operator<<(ostream&, int n);
ostream& operator<<(ostream&, long n);
ostream& operator<<(ostream&, unsigned int n);
ostream& operator<<(ostream&, unsigned long n);
ostream& operator<<(ostream& os, short n) {return os << (int)n;}
ostream& operator<<(ostream& os, unsigned short n)
{return os << (unsigned int)n;}
ostream& operator<<(ostream&, float n);
ostream& operator<<(ostream&, double n);
ostream& operator<<(ostream& os, __omanip func) { return (*func)(os); }
ostream& operator<<(ostream&, streambuf*);

class istream : public ios
{
    size_t _gcount;
  public:
    istream();
    istream(streambuf* sb, ostream*tied=0 );
    ~istream();
    streambuf* istreambuf() const { return _strbuf; }
    istream& get(char& c);
    istream& get(unsigned char& c);
    istream& read(char *ptr, int n);
    istream& read(unsigned char *ptr, int n) { return read((char*)ptr, n); }
    istream& read(void *ptr, int n) { return read((char*)ptr, n); }
    int get() { return _strbuf->sbumpc(); }
    istream& getline(char* ptr, int len, char delim = '\n');
    istream& get(char* ptr, int len, char delim = '\n');
    istream& gets(char **s, char delim = '\n');
    int ipfx(int need) {
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie && (need == 0 || rdbuf()->in_avail())) ; /* THIS IS BAD */
	if (!need && (flags() & ios::skipws) && !ws(*this)) return 0;
	return 1;
    }
    int ipfx0() { // Optimized version of ipfx(0).
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie) _tie->flush();
	if ((flags() & ios::skipws) && !ws(*this)) return 0;
	return 1;
    }
    int ipfx1() { // Optimized version of ipfx(1).
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie && rdbuf()->in_avail() == 0) _tie->flush();
	return 1;
    }
    size_t gcount() { return _gcount; }
    istream& seekg(streampos);
    istream& seekg(streamoff, _seek_dir);
    streampos tellg();
    istream& putback(char ch) {
	if (good() && _strbuf->sputbackc(ch) == (-1) ) clear(ios::badbit);
	return *this;}
    istream& unget() {
	if (good() && _strbuf->sungetc() == (-1) ) clear(ios::badbit);
	return *this;}

    istream& unget(char ch) { return putback(ch); }
    int skip(int i);

};

istream& operator>>(istream&, char*);
istream& operator>>(istream& is, unsigned char* p) { return is >> (char*)p; }
//istream& operator>>(istream& is, signed char* p) { return is >> (char*)p; }
istream& operator>>(istream&, char& c);
istream& operator>>(istream&, unsigned char& c);
//istream& operator>>(istream&, signed char& c);
istream& operator>>(istream&, int&);
istream& operator>>(istream&, long&);
istream& operator>>(istream&, short&);
istream& operator>>(istream&, unsigned int&);
istream& operator>>(istream&, unsigned long&);
istream& operator>>(istream&, unsigned short&);
istream& operator>>(istream&, float&);
istream& operator>>(istream&, double&);
istream& operator>>(istream& is, __imanip func) { return (*func)(is); }

class iostream : public ios {
    size_t _gcount;
  public:
    iostream();
    operator istream&() { return *(istream*)this; }
    operator ostream&() { return *(ostream*)this; }
    ~iostream();
    // NOTE: These duplicate istream methods.
    istream& get(char& c) { return ((istream*)this)->get(c); }
    istream& get(unsigned char& c) { return ((istream*)this)->get(c); }
    istream& read(char *ptr, int n) { return ((istream*)this)->read(ptr, n); }
    istream& read(unsigned char *ptr, int n)
	{ return ((istream*)this)->read((char*)ptr, n); }
    istream& read(void *ptr, int n)
	{ return ((istream*)this)->read((char*)ptr, n); }
    int get() { return _strbuf->sbumpc(); }
    istream& getline(char* ptr, int len, char delim = '\n')
	{ return ((istream*)this)->getline(ptr, len, delim); }
    istream& get(char* ptr, int len, char delim = '\n')
	{ return ((istream*)this)->get(ptr, len, delim); }
    istream& gets(char **s, char delim = '\n')
	{ return ((istream*)this)->gets(s, delim); }
    int ipfx(int need) { return ((istream*)this)->ipfx(need); }
    int ipfx0()  { return ((istream*)this)->ipfx0(); }
    int ipfx1()  { return ((istream*)this)->ipfx1(); }
    size_t gcount() { return _gcount; }
    istream& putback(char ch) { return ((istream*)this)->putback(ch); }
    istream& unget() { return ((istream*)this)->unget(); }
    istream& seekg(streampos pos) { return ((istream*)this)->seekg(pos); }
    istream& seekg(streamoff off, _seek_dir dir)
	{ return ((istream*)this)->seekg(off, dir); }
    streampos tellg() { return ((istream*)this)->tellg(); }

    istream& unget(char ch) { return putback(ch); }


    // NOTE: These duplicate ostream methods.
    int opfx() { return ((ostream*)this)->opfx(); }
    void osfx() { ((ostream*)this)->osfx(); }
    ostream& flush() { return ((ostream*)this)->flush(); }
    ostream& put(char c) { return ((ostream*)this)->put(c); }
    ostream& write(const char *s, int n)
	{ return ((ostream*)this)->write(s, n); }
    ostream& write(const unsigned char *s, int n)
	{ return ((ostream*)this)->write((char*)s, n); }
    ostream& write(const void *s, int n)
	{ return ((ostream*)this)->write((char*)s, n); }
    ostream& form(const char *format ...);
    ostream& vform(const char *format, char*  args)
	{ return ((ostream*)this)->vform(format, args); }
    ostream& seekp(streampos pos) { return ((ostream*)this)->seekp(pos); }
    ostream& seekp(streamoff off, _seek_dir dir)
	{ return ((ostream*)this)->seekp(off, dir); }
    streampos tellp() { return ((ostream*)this)->tellp(); }
};

extern istream cin;
extern ostream cout, cerr, clog; // clog->rdbuf() == cerr->rdbuf()

inline ostream& ostream::put(char c) { _strbuf->sputc(c); return *this; }

struct Iostream_init { } ;  // Compatibility hack for AT&T libraray.


//# 7 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/stream.h" 2


extern char* form(char*, ...);

extern char* dec(long, int=0);
extern char* dec(int, int=0);
extern char* dec(unsigned long, int=0);
extern char* dec(unsigned int, int=0);

extern char* hex(long, int=0);
extern char* hex(int, int=0);
extern char* hex(unsigned long, int=0);
extern char* hex(unsigned int, int=0);

extern char* oct(long, int=0);
extern char* oct(int, int=0);
extern char* oct(unsigned long, int=0);
extern char* oct(unsigned int, int=0);

inline istream& WS(istream& str) { return ws(str); }


//# 26 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/String.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/Regex.h" 1
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/




//#pragma interface









struct re_pattern_buffer;       // defined elsewhere
struct re_registers;

class Regex
{
private:

                     Regex(const Regex&) {}  // no X(X&)
  void               operator = (const Regex&) {} // no assignment

protected:
  re_pattern_buffer* buf;
  re_registers*      reg;

public:
                     Regex(const char* t,
                           int fast = 0,
                           int bufsize = 40,
                           const char* transtable = 0);

                    ~Regex();

  int                match(const char* s, int len, int pos = 0) const;
  int                search(const char* s, int len,
                            int& matchlen, int startpos = 0) const;
  int                match_info(int& start, int& length, int nth = 0) const;

  int                OK() const;  // representation invariant
};

// some built in regular expressions

extern const Regex RXwhite;          // = "[ \n\t\r\v\f]+"
extern const Regex RXint;            // = "-?[0-9]+"
extern const Regex RXdouble;         // = "-?\\(\\([0-9]+\\.[0-9]*\\)\\|
                                     //    \\([0-9]+\\)\\|\\(\\.[0-9]+\\)\\)
                                     //    \\([eE][---+]?[0-9]+\\)?"
extern const Regex RXalpha;          // = "[A-Za-z]+"
extern const Regex RXlowercase;      // = "[a-z]+"
extern const Regex RXuppercase;      // = "[A-Z]+"
extern const Regex RXalphanum;       // = "[0-9A-Za-z]+"
extern const Regex RXidentifier;     // = "[A-Za-z_][A-Za-z0-9_]*"



//# 27 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/String.h" 2


struct StrRep                     // internal String representations
{
  unsigned short    len;         // string length
  unsigned short    sz;          // allocated space
  char              s[1];        // the string starts here
                                 // (at least 1 char for trailing null)
                                 // allocated & expanded via non-public fcts
};

// primitive ops on StrReps -- nearly all String fns go through these.

StrRep*     Salloc(StrRep*, const char*, int, int);
StrRep*     Scopy(StrRep*, StrRep*);
StrRep*     Sresize(StrRep*, int);
StrRep*     Scat(StrRep*, const char*, int, const char*, int);
StrRep*     Scat(StrRep*, const char*, int,const char*,int, const char*,int);
StrRep*     Sprepend(StrRep*, const char*, int);
StrRep*     Sreverse(StrRep*, StrRep*);
StrRep*     Supcase(StrRep*, StrRep*);
StrRep*     Sdowncase(StrRep*, StrRep*);
StrRep*     Scapitalize(StrRep*, StrRep*);

// These classes need to be defined in the order given

class String;
class SubString;

class SubString
{
  friend class      String;
protected:

  String&           S;        // The String I'm a substring of
  unsigned short    pos;      // starting position in S's rep
  unsigned short    len;      // length of substring

  void              assign(StrRep*, const char*, int = -1);
                    SubString(String& x, int p, int l);
                    SubString(const SubString& x);

public:

// Note there are no public constructors. SubStrings are always
// created via String operations

                   ~SubString();

  void              operator =  (const String&     y);
  void              operator =  (const SubString&  y);
  void              operator =  (const char* t);
  void              operator =  (char        c);

// return 1 if target appears anywhere in SubString; else 0

  int               contains(char        c) const;
  int               contains(const String&     y) const;
  int               contains(const SubString&  y) const;
  int               contains(const char* t) const;
  int               contains(const Regex&       r) const;

// return 1 if target matches entire SubString

  int               matches(const Regex&  r) const;

// IO

  friend ostream&   operator<<(ostream& s, const SubString& x);

// status

  unsigned int      length() const;
  int               empty() const;
  const char*       chars() const;

  int               OK() const;

};


class String
{
  friend class      SubString;

protected:
  StrRep*           rep;   // Strings are pointers to their representations

// some helper functions

  int               search(int, int, const char*, int = -1) const;
  int               search(int, int, char) const;
  int               match(int, int, int, const char*, int = -1) const;
  int               _gsub(const char*, int, const char* ,int);
  int               _gsub(const Regex&, const char*, int);
  SubString         _substr(int, int);

public:

// constructors & assignment

                    String();
                    String(const String& x);
                    String(const SubString&  x);
                    String(const char* t);
                    String(const char* t, int len);
                    String(char c);

                    ~String();

  void              operator =  (const String&     y);
  void              operator =  (const char* y);
  void              operator =  (char        c);
  void              operator =  (const SubString&  y);

// concatenation

  void              operator += (const String&     y);
  void              operator += (const SubString&  y);
  void              operator += (const char* t);
  void              operator += (char        c);

  void              prepend(const String&     y);
  void              prepend(const SubString&  y);
  void              prepend(const char* t);
  void              prepend(char        c);


// procedural versions:
// concatenate first 2 args, store result in last arg

  friend void     cat(const String&, const String&, String&);
  friend void     cat(const String&, const SubString&, String&);
  friend void     cat(const String&, const char*, String&);
  friend void     cat(const String&, char, String&);

  friend void     cat(const SubString&, const String&, String&);
  friend void     cat(const SubString&, const SubString&, String&);
  friend void     cat(const SubString&, const char*, String&);
  friend void     cat(const SubString&, char, String&);

  friend void     cat(const char*, const String&, String&);
  friend void     cat(const char*, const SubString&, String&);
  friend void     cat(const char*, const char*, String&);
  friend void     cat(const char*, char, String&);

// double concatenation, by request. (yes, there are too many versions,
// but if one is supported, then the others should be too...)
// Concatenate first 3 args, store in last arg

  friend void     cat(const String&,const String&, const String&,String&);
  friend void     cat(const String&,const String&,const SubString&,String&);
  friend void     cat(const String&,const String&, const char*, String&);
  friend void     cat(const String&,const String&, char, String&);
  friend void     cat(const String&,const SubString&,const String&,String&);
  friend void     cat(const String&,const SubString&,const SubString&,String&);
  friend void     cat(const String&,const SubString&, const char*, String&);
  friend void     cat(const String&,const SubString&, char, String&);
  friend void     cat(const String&,const char*, const String&,    String&);
  friend void     cat(const String&,const char*, const SubString&, String&);
  friend void     cat(const String&,const char*, const char*, String&);
  friend void     cat(const String&,const char*, char, String&);

  friend void     cat(const char*, const String&, const String&,String&);
  friend void     cat(const char*,const String&,const SubString&,String&);
  friend void     cat(const char*,const String&, const char*, String&);
  friend void     cat(const char*,const String&, char, String&);
  friend void     cat(const char*,const SubString&,const String&,String&);
  friend void     cat(const char*,const SubString&,const SubString&,String&);
  friend void     cat(const char*,const SubString&, const char*, String&);
  friend void     cat(const char*,const SubString&, char, String&);
  friend void     cat(const char*,const char*, const String&,    String&);
  friend void     cat(const char*,const char*, const SubString&, String&);
  friend void     cat(const char*,const char*, const char*, String&);
  friend void     cat(const char*,const char*, char, String&);


// searching & matching

// return position of target in string or -1 for failure

  int               index(char        c, int startpos = 0) const;
  int               index(const String&     y, int startpos = 0) const;
  int               index(const SubString&  y, int startpos = 0) const;
  int               index(const char* t, int startpos = 0) const;
  int               index(const Regex&      r, int startpos = 0) const;

// return 1 if target appears anyhere in String; else 0

  int               contains(char        c) const;
  int               contains(const String&     y) const;
  int               contains(const SubString&  y) const;
  int               contains(const char* t) const;
  int               contains(const Regex&      r) const;

// return 1 if target appears anywhere after position pos
// (or before, if pos is negative) in String; else 0

  int               contains(char        c, int pos) const;
  int               contains(const String&     y, int pos) const;
  int               contains(const SubString&  y, int pos) const;
  int               contains(const char* t, int pos) const;
  int               contains(const Regex&      r, int pos) const;

// return 1 if target appears at position pos in String; else 0

  int               matches(char        c, int pos = 0) const;
  int               matches(const String&     y, int pos = 0) const;
  int               matches(const SubString&  y, int pos = 0) const;
  int               matches(const char* t, int pos = 0) const;
  int               matches(const Regex&      r, int pos = 0) const;

//  return number of occurences of target in String

  int               freq(char        c) const;
  int               freq(const String&     y) const;
  int               freq(const SubString&  y) const;
  int               freq(const char* t) const;

// SubString extraction

// Note that you can't take a substring of a const String, since
// this leaves open the possiblility of indirectly modifying the
// String through the SubString

  SubString         at(int         pos, int len);
  SubString         operator () (int         pos, int len); // synonym for at

  SubString         at(const String&     x, int startpos = 0);
  SubString         at(const SubString&  x, int startpos = 0);
  SubString         at(const char* t, int startpos = 0);
  SubString         at(char        c, int startpos = 0);
  SubString         at(const Regex&      r, int startpos = 0);

  SubString         before(int          pos);
  SubString         before(const String&      x, int startpos = 0);
  SubString         before(const SubString&   x, int startpos = 0);
  SubString         before(const char*  t, int startpos = 0);
  SubString         before(char         c, int startpos = 0);
  SubString         before(const Regex&       r, int startpos = 0);

  SubString         through(int          pos);
  SubString         through(const String&      x, int startpos = 0);
  SubString         through(const SubString&   x, int startpos = 0);
  SubString         through(const char*  t, int startpos = 0);
  SubString         through(char         c, int startpos = 0);
  SubString         through(const Regex&       r, int startpos = 0);

  SubString         from(int          pos);
  SubString         from(const String&      x, int startpos = 0);
  SubString         from(const SubString&   x, int startpos = 0);
  SubString         from(const char*  t, int startpos = 0);
  SubString         from(char         c, int startpos = 0);
  SubString         from(const Regex&       r, int startpos = 0);

  SubString         after(int         pos);
  SubString         after(const String&     x, int startpos = 0);
  SubString         after(const SubString&  x, int startpos = 0);
  SubString         after(const char* t, int startpos = 0);
  SubString         after(char        c, int startpos = 0);
  SubString         after(const Regex&      r, int startpos = 0);


// deletion

// delete len chars starting at pos
  void              del(int         pos, int len);

// delete the first occurrence of target after startpos

  void              del(const String&     y, int startpos = 0);
  void              del(const SubString&  y, int startpos = 0);
  void              del(const char* t, int startpos = 0);
  void              del(char        c, int startpos = 0);
  void              del(const Regex&      r, int startpos = 0);

// global substitution: substitute all occurrences of pat with repl

  int               gsub(const String&     pat, const String&     repl);
  int               gsub(const SubString&  pat, const String&     repl);
  int               gsub(const char* pat, const String&     repl);
  int               gsub(const char* pat, const char* repl);
  int               gsub(const Regex&      pat, const String&     repl);

// friends & utilities

// split string into array res at separators; return number of elements

  friend int        split(const String& x, String res[], int maxn,
                          const String& sep);
  friend int        split(const String& x, String res[], int maxn,
                          const Regex&  sep);

  friend String     common_prefix(const String& x, const String& y,
                                  int startpos = 0);
  friend String     common_suffix(const String& x, const String& y,
                                  int startpos = -1);
  friend String     replicate(char        c, int n);
  friend String     replicate(const String&     y, int n);
  friend String     join(String src[], int n, const String& sep);

// simple builtin transformations

  friend String     reverse(const String& x);
  friend String     upcase(const String& x);
  friend String     downcase(const String& x);
  friend String     capitalize(const String& x);

// in-place versions of above

  void              reverse();
  void              upcase();
  void              downcase();
  void              capitalize();

// element extraction

  char&             operator [] (int i);
  char              elem(int i) const;
  char              firstchar() const;
  char              lastchar() const;

// conversion

                    operator const char*() const;
  const char*       chars() const;


// IO

  friend ostream&   operator<<(ostream& s, const String& x);
  friend ostream&   operator<<(ostream& s, const SubString& x);
  friend istream&   operator>>(istream& s, String& x);

  friend int        readline(istream& s, String& x,
                             char terminator = '\n',
                             int discard_terminator = 1);

// status

  unsigned int      length() const;
  int               empty() const;

// preallocate some space for String
  void              alloc(int newsize);

// report current allocation (not length!)

  int               allocation() const;


  volatile void     error(const char* msg) const;

  int               OK() const;
};

typedef String StrTmp; // for backward compatibility

// other externs

int        compare(const String&    x, const String&     y);
int        compare(const String&    x, const SubString&  y);
int        compare(const String&    x, const char* y);
int        compare(const SubString& x, const String&     y);
int        compare(const SubString& x, const SubString&  y);
int        compare(const SubString& x, const char* y);
int        fcompare(const String&   x, const String&     y); // ignore case

extern StrRep  _nilStrRep;
extern String _nilString;

// other inlines

String operator + (const String& x, const String& y);
String operator + (const String& x, const SubString& y);
String operator + (const String& x, const char* y);
String operator + (const String& x, char y);
String operator + (const SubString& x, const String& y);
String operator + (const SubString& x, const SubString& y);
String operator + (const SubString& x, const char* y);
String operator + (const SubString& x, char y);
String operator + (const char* x, const String& y);
String operator + (const char* x, const SubString& y);

int operator==(const String& x, const String& y);
int operator!=(const String& x, const String& y);
int operator> (const String& x, const String& y);
int operator>=(const String& x, const String& y);
int operator< (const String& x, const String& y);
int operator<=(const String& x, const String& y);
int operator==(const String& x, const SubString&  y);
int operator!=(const String& x, const SubString&  y);
int operator> (const String& x, const SubString&  y);
int operator>=(const String& x, const SubString&  y);
int operator< (const String& x, const SubString&  y);
int operator<=(const String& x, const SubString&  y);
int operator==(const String& x, const char* t);
int operator!=(const String& x, const char* t);
int operator> (const String& x, const char* t);
int operator>=(const String& x, const char* t);
int operator< (const String& x, const char* t);
int operator<=(const String& x, const char* t);
int operator==(const SubString& x, const String& y);
int operator!=(const SubString& x, const String& y);
int operator> (const SubString& x, const String& y);
int operator>=(const SubString& x, const String& y);
int operator< (const SubString& x, const String& y);
int operator<=(const SubString& x, const String& y);
int operator==(const SubString& x, const SubString&  y);
int operator!=(const SubString& x, const SubString&  y);
int operator> (const SubString& x, const SubString&  y);
int operator>=(const SubString& x, const SubString&  y);
int operator< (const SubString& x, const SubString&  y);
int operator<=(const SubString& x, const SubString&  y);
int operator==(const SubString& x, const char* t);
int operator!=(const SubString& x, const char* t);
int operator> (const SubString& x, const char* t);
int operator>=(const SubString& x, const char* t);
int operator< (const SubString& x, const char* t);
int operator<=(const SubString& x, const char* t);




// status reports, needed before defining other things

inline unsigned int String::length() const {  return rep->len; }
inline int         String::empty() const { return rep->len == 0; }
inline const char* String::chars() const { return &(rep->s[0]); }
inline int         String::allocation() const { return rep->sz; }
inline void        String::alloc(int newsize) { rep = Sresize(rep, newsize); }

inline unsigned int SubString::length() const { return len; }
inline int         SubString::empty() const { return len == 0; }
inline const char* SubString::chars() const { return &(S.rep->s[pos]); }


// constructors

inline String::String()
  : rep(&_nilStrRep) {}
inline String::String(const String& x)
  : rep(Scopy(0, x.rep)) {}
inline String::String(const char* t)
  : rep(Salloc(0, t, -1, -1)) {}
inline String::String(const char* t, int tlen)
  : rep(Salloc(0, t, tlen, tlen)) {}
inline String::String(const SubString& y)
  : rep(Salloc(0, y.chars(), y.length(), y.length())) {}
inline String::String(char c)
  : rep(Salloc(0, &c, 1, 1)) {}

inline String::~String() { if (rep != &_nilStrRep) delete rep; }

inline SubString::SubString(const SubString& x)
  :S(x.S), pos(x.pos), len(x.len) {}
inline SubString::SubString(String& x, int first, int l)
  :S(x), pos(first), len(l) {}

inline SubString::~SubString() {}

// assignment

inline void String::operator =  (const String& y)
{
  rep = Scopy(rep, y.rep);
}

inline void String::operator=(const char* t)
{
  rep = Salloc(rep, t, -1, -1);
}

inline void String::operator=(const SubString&  y)
{
  rep = Salloc(rep, y.chars(), y.length(), y.length());
}

inline void String::operator=(char c)
{
  rep = Salloc(rep, &c, 1, 1);
}


inline void SubString::operator = (const char* ys)
{
  assign(0, ys);
}

inline void SubString::operator = (char ch)
{
  assign(0, &ch, 1);
}

inline void SubString::operator = (const String& y)
{
  assign(y.rep, y.chars(), y.length());
}

inline void SubString::operator = (const SubString& y)
{
  assign(y.S.rep, y.chars(), y.length());
}

// Zillions of cats...

inline void cat(const String& x, const String& y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String& x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String& x, const char* y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y, -1);
}

inline void cat(const String& x, char y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), &y, 1);
}

inline void cat(const SubString& x, const String& y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const SubString& x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const SubString& x, const char* y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y, -1);
}

inline void cat(const SubString& x, char y, String& r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), &y, 1);
}

inline void cat(const char* x, const String& y, String& r)
{
  r.rep = Scat(r.rep, x, -1, y.chars(), y.length());
}

inline void cat(const char* x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, x, -1, y.chars(), y.length());
}

inline void cat(const char* x, const char* y, String& r)
{
  r.rep = Scat(r.rep, x, -1, y, -1);
}

inline void cat(const char* x, char y, String& r)
{
  r.rep = Scat(r.rep, x, -1, &y, 1);
}

inline void cat(const String& a, const String& x, const String& y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String& a, const String& x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String& a, const String& x, const char* y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y, -1);
}

inline void cat(const String& a, const String& x, char y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), &y, 1);
}

inline void cat(const String& a, const SubString& x, const String& y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String& a, const SubString& x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String& a, const SubString& x, const char* y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y, -1);
}

inline void cat(const String& a, const SubString& x, char y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), &y, 1);
}

inline void cat(const String& a, const char* x, const String& y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, y.chars(), y.length());
}

inline void cat(const String& a, const char* x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, y.chars(), y.length());
}

inline void cat(const String& a, const char* x, const char* y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, y, -1);
}

inline void cat(const String& a, const char* x, char y, String& r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, &y, 1);
}


inline void cat(const char* a, const String& x, const String& y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char* a, const String& x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char* a, const String& x, const char* y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y, -1);
}

inline void cat(const char* a, const String& x, char y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), &y, 1);
}

inline void cat(const char* a, const SubString& x, const String& y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char* a, const SubString& x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char* a, const SubString& x, const char* y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y, -1);
}

inline void cat(const char* a, const SubString& x, char y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), &y, 1);
}

inline void cat(const char* a, const char* x, const String& y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y.chars(), y.length());
}

inline void cat(const char* a, const char* x, const SubString& y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y.chars(), y.length());
}

inline void cat(const char* a, const char* x, const char* y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y, -1);
}

inline void cat(const char* a, const char* x, char y, String& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, &y, 1);
}


// operator versions

inline void String::operator +=(const String& y)
{
  cat(*this, y, *this);
}

inline void String::operator +=(const SubString& y)
{
  cat(*this, y, *this);
}

inline void String::operator += (const char* y)
{
  cat(*this, y, *this);
}

inline void String:: operator +=(char y)
{
  cat(*this, y, *this);
}

// constructive concatenation



inline String operator + (const String& x, const String& y) return r;
{
  cat(x, y, r);
}

inline String operator + (const String& x, const SubString& y) return r;
{
  cat(x, y, r);
}

inline String operator + (const String& x, const char* y) return r;
{
  cat(x, y, r);
}

inline String operator + (const String& x, char y) return r;
{
  cat(x, y, r);
}

inline String operator + (const SubString& x, const String& y) return r;
{
  cat(x, y, r);
}

inline String operator + (const SubString& x, const SubString& y) return r;
{
  cat(x, y, r);
}

inline String operator + (const SubString& x, const char* y) return r;
{
  cat(x, y, r);
}

inline String operator + (const SubString& x, char y) return r;
{
  cat(x, y, r);
}

inline String operator + (const char* x, const String& y) return r;
{
  cat(x, y, r);
}

inline String operator + (const char* x, const SubString& y) return r;
{
  cat(x, y, r);
}

inline String reverse(const String& x) return r;
{
  r.rep = Sreverse(x.rep, r.rep);
}

inline String upcase(const String& x) return r;
{
  r.rep = Supcase(x.rep, r.rep);
}

inline String downcase(const String& x) return r;
{
  r.rep = Sdowncase(x.rep, r.rep);
}

inline String capitalize(const String& x) return r;
{
  r.rep = Scapitalize(x.rep, r.rep);
}

//# 883 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/String.h"


// prepend

inline void String::prepend(const String& y)
{
  rep = Sprepend(rep, y.chars(), y.length());
}

inline void String::prepend(const char* y)
{
  rep = Sprepend(rep, y, -1);
}

inline void String::prepend(char y)
{
  rep = Sprepend(rep, &y, 1);
}

inline void String::prepend(const SubString& y)
{
  rep = Sprepend(rep, y.chars(), y.length());
}

// misc transformations


inline void String::reverse()
{
  rep = Sreverse(rep, rep);
}


inline void String::upcase()
{
  rep = Supcase(rep, rep);
}


inline void String::downcase()
{
  rep = Sdowncase(rep, rep);
}


inline void String::capitalize()
{
  rep = Scapitalize(rep, rep);
}

// element extraction

inline char&  String::operator [] (int i)
{
  if (((unsigned)i) >= length()) error("invalid index");
  return rep->s[i];
}

inline char  String::elem (int i) const
{
  if (((unsigned)i) >= length()) error("invalid index");
  return rep->s[i];
}

inline char  String::firstchar() const
{
  return elem(0);
}

inline char  String::lastchar() const
{
  return elem(length() - 1);
}

// searching

inline int String::index(char c, int startpos) const
{
  return search(startpos, length(), c);
}

inline int String::index(const char* t, int startpos) const
{
  return search(startpos, length(), t);
}

inline int String::index(const String& y, int startpos) const
{
  return search(startpos, length(), y.chars(), y.length());
}

inline int String::index(const SubString& y, int startpos) const
{
  return search(startpos, length(), y.chars(), y.length());
}

inline int String::index(const Regex& r, int startpos) const
{
  int unused;  return r.search(chars(), length(), unused, startpos);
}

inline int String::contains(char c) const
{
  return search(0, length(), c) >= 0;
}

inline int String::contains(const char* t) const
{
  return search(0, length(), t) >= 0;
}

inline int String::contains(const String& y) const
{
  return search(0, length(), y.chars(), y.length()) >= 0;
}

inline int String::contains(const SubString& y) const
{
  return search(0, length(), y.chars(), y.length()) >= 0;
}

inline int String::contains(char c, int p) const
{
  return match(p, length(), 0, &c, 1) >= 0;
}

inline int String::contains(const char* t, int p) const
{
  return match(p, length(), 0, t) >= 0;
}

inline int String::contains(const String& y, int p) const
{
  return match(p, length(), 0, y.chars(), y.length()) >= 0;
}

inline int String::contains(const SubString& y, int p) const
{
  return match(p, length(), 0, y.chars(), y.length()) >= 0;
}

inline int String::contains(const Regex& r) const
{
  int unused;  return r.search(chars(), length(), unused, 0) >= 0;
}

inline int String::contains(const Regex& r, int p) const
{
  return r.match(chars(), length(), p) >= 0;
}


inline int String::matches(const SubString& y, int p) const
{
  return match(p, length(), 1, y.chars(), y.length()) >= 0;
}

inline int String::matches(const String& y, int p) const
{
  return match(p, length(), 1, y.chars(), y.length()) >= 0;
}

inline int String::matches(const char* t, int p) const
{
  return match(p, length(), 1, t) >= 0;
}

inline int String::matches(char c, int p) const
{
  return match(p, length(), 1, &c, 1) >= 0;
}

inline int String::matches(const Regex& r, int p) const
{
  int l = (p < 0)? -p : length() - p;
  return r.match(chars(), length(), p) == l;
}


inline int SubString::contains(const char* t) const
{
  return S.search(pos, pos+len, t) >= 0;
}

inline int SubString::contains(const String& y) const
{
  return S.search(pos, pos+len, y.chars(), y.length()) >= 0;
}

inline int SubString::contains(const SubString&  y) const
{
  return S.search(pos, pos+len, y.chars(), y.length()) >= 0;
}

inline int SubString::contains(char c) const
{
  return S.search(pos, pos+len, 0, c) >= 0;
}

inline int SubString::contains(const Regex& r) const
{
  int unused;  return r.search(chars(), len, unused, 0) >= 0;
}

inline int SubString::matches(const Regex& r) const
{
  return r.match(chars(), len, 0) == len;
}


inline int String::gsub(const String& pat, const String& r)
{
  return _gsub(pat.chars(), pat.length(), r.chars(), r.length());
}

inline int String::gsub(const SubString&  pat, const String& r)
{
  return _gsub(pat.chars(), pat.length(), r.chars(), r.length());
}

inline int String::gsub(const Regex& pat, const String& r)
{
  return _gsub(pat, r.chars(), r.length());
}

inline int String::gsub(const char* pat, const String& r)
{
  return _gsub(pat, -1, r.chars(), r.length());
}

inline int String::gsub(const char* pat, const char* r)
{
  return _gsub(pat, -1, r, -1);
}



inline  ostream& operator<<(ostream& s, const String& x)
{
   s << x.chars(); return s;
}

// a zillion comparison operators

inline int operator==(const String& x, const String& y)
{
  return compare(x, y) == 0;
}

inline int operator!=(const String& x, const String& y)
{
  return compare(x, y) != 0;
}

inline int operator>(const String& x, const String& y)
{
  return compare(x, y) > 0;
}

inline int operator>=(const String& x, const String& y)
{
  return compare(x, y) >= 0;
}

inline int operator<(const String& x, const String& y)
{
  return compare(x, y) < 0;
}

inline int operator<=(const String& x, const String& y)
{
  return compare(x, y) <= 0;
}

inline int operator==(const String& x, const SubString&  y)
{
  return compare(x, y) == 0;
}

inline int operator!=(const String& x, const SubString&  y)
{
  return compare(x, y) != 0;
}

inline int operator>(const String& x, const SubString&  y)
{
  return compare(x, y) > 0;
}

inline int operator>=(const String& x, const SubString&  y)
{
  return compare(x, y) >= 0;
}

inline int operator<(const String& x, const SubString&  y)
{
  return compare(x, y) < 0;
}

inline int operator<=(const String& x, const SubString&  y)
{
  return compare(x, y) <= 0;
}

inline int operator==(const String& x, const char* t)
{
  return compare(x, t) == 0;
}

inline int operator!=(const String& x, const char* t)
{
  return compare(x, t) != 0;
}

inline int operator>(const String& x, const char* t)
{
  return compare(x, t) > 0;
}

inline int operator>=(const String& x, const char* t)
{
  return compare(x, t) >= 0;
}

inline int operator<(const String& x, const char* t)
{
  return compare(x, t) < 0;
}

inline int operator<=(const String& x, const char* t)
{
  return compare(x, t) <= 0;
}

inline int operator==(const SubString& x, const String& y)
{
  return compare(y, x) == 0;
}

inline int operator!=(const SubString& x, const String& y)
{
  return compare(y, x) != 0;
}

inline int operator>(const SubString& x, const String& y)
{
  return compare(y, x) < 0;
}

inline int operator>=(const SubString& x, const String& y)
{
  return compare(y, x) <= 0;
}

inline int operator<(const SubString& x, const String& y)
{
  return compare(y, x) > 0;
}

inline int operator<=(const SubString& x, const String& y)
{
  return compare(y, x) >= 0;
}

inline int operator==(const SubString& x, const SubString&  y)
{
  return compare(x, y) == 0;
}

inline int operator!=(const SubString& x, const SubString&  y)
{
  return compare(x, y) != 0;
}

inline int operator>(const SubString& x, const SubString&  y)
{
  return compare(x, y) > 0;
}

inline int operator>=(const SubString& x, const SubString&  y)
{
  return compare(x, y) >= 0;
}

inline int operator<(const SubString& x, const SubString&  y)
{
  return compare(x, y) < 0;
}

inline int operator<=(const SubString& x, const SubString&  y)
{
  return compare(x, y) <= 0;
}

inline int operator==(const SubString& x, const char* t)
{
  return compare(x, t) == 0;
}

inline int operator!=(const SubString& x, const char* t)
{
  return compare(x, t) != 0;
}

inline int operator>(const SubString& x, const char* t)
{
  return compare(x, t) > 0;
}

inline int operator>=(const SubString& x, const char* t)
{
  return compare(x, t) >= 0;
}

inline int operator<(const SubString& x, const char* t)
{
  return compare(x, t) < 0;
}

inline int operator<=(const SubString& x, const char* t)
{
  return compare(x, t) <= 0;
}


// a helper needed by at, before, etc.

inline SubString String::_substr(int first, int l)
{
  if (first  >= length() )
    return SubString(_nilString, 0, 0) ;
  else
    return SubString(*this, first, l);
}





//# 26 "../../../../mips/include/Gct/GctErrorHandler.h" 2

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 27 "../../../../mips/include/Gct/GctErrorHandler.h" 2


//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/stuff++.h" 1
// -*- C++ -*-



//
// Fake up a libstuff++
//
// This is done as a complete and utter hack; this library has no function
// at all being in the boot area; it is here solely in order to provide a
// libstuff++ against which the Makefiles can resolve link lines.
//
// The only reason that this is done is to allow the STANDARD_C++_LIBRARIES
// as provided by the Makefile templates in the boot area to be the same
// ones that are used by the tools outside this hierarchy.
//
// The tools outside this hierarchy use a different libstuff++; one that is
// written in C++.  This one is not written in C++ in order to be simpler.
//





//#pragma interface



extern "C" {
//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/stuff.h" 1



/*
 * Useful stuff
 */

/*
 */

//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/ansi.h" 1




/*
 * ANSI Compiler Support
 *
 * David Harrison
 * University of California, Berkeley
 * 1988
 *
 * ANSI compatible compilers are supposed to define the preprocessor
 * directive __STDC__.  Based on this directive, this file defines
 * certain ANSI specific macros.
 *
 * ARGS:
 *   Used in function prototypes.  Example:
 *   extern int foo
 *     ARGS((char *blah, double threshold));
 */

/*
 *
 * Modifications
 * Wendell C Baker
 * University of California, Berkeley
 */

/* Function prototypes */



























//# 15 "/sandbox/wbaker/wbaker0/source/mips/include/stuff.h" 2



/*
 * If g++, then we stub out part of this thing and let the C++ types take
 * over and do the same job; some compatibility must be given however
 */

/*
 *    Use the GNU libg++ definition
 */
//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/bool.h" 1







//# 26 "/sandbox/wbaker/wbaker0/source/mips/include/stuff.h" 2

//# 35 "/sandbox/wbaker/wbaker0/source/mips/include/stuff.h"


/*
 * Make various pieces of C code that use the old ``Boolean''
 * be compatible by faking up the definition of Boolean using
 * the new bool type.
 */


//# 58 "/sandbox/wbaker/wbaker0/source/mips/include/stuff.h"


typedef long FitAny;		/* can fit any integral type */

/*
 * typedef char *String;	- DO NOT USE THIS - it conflicts with C++
 * typedef char **Stringv;	- just use char* and char** instead.
 * 				- void* can be used for arbitrary pointers
 */




extern int nocase_strcmp (char *, char *)		;
extern int nocase_strncmp (char *, char *, int)		;

extern bool	 nocase_strequal (char *, char *)		;
extern bool	 nocase_strnequal (char *, char *, int)		;

extern bool	 lead_strequal (char *, char *)		;
extern bool	 nocase_lead_strequal (char *, char *)		;

extern int strhash (char *, int)		;
extern int nocase_strhash (char *, int)		;

extern int sign (int)		;

/*
 *    Some useful macros.
 */





























//# 33 "/sandbox/wbaker/wbaker0/source/mips/include/stuff++.h" 2

}

//
// This is here because we wish to provide externs for the two
// functions btoa(bool, unsigned = 0) and operator<<(ostream&, bool)
// because they are not provided in bool.h.
//
//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/bool.h" 1







//# 41 "/sandbox/wbaker/wbaker0/source/mips/include/stuff++.h" 2

extern const char *stringify(bool b);
//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 43 "/sandbox/wbaker/wbaker0/source/mips/include/stuff++.h" 2

extern ostream& operator<<(ostream&, bool);

// Should this be kept separate?   bool isn't, but then  is
// included here only to define ostream& operator<<(ostream&, bool)
//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/unit.h" 1
// -*- C++ -*-



//
// unit enum
//
// Wendell Baker, Berkeley CAD Group, 1991 (wbaker@ic.Berkeley.EDU)
//


//
// unit enum
//
// This _looks_ silly, but it has an important theoretical basis in category
// theory.  For the pragmatic reason for its existence, see the example below.
//
enum unit {
    UNIT = 1,
};

extern const char *stringify(unit u);

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 28 "/sandbox/wbaker/wbaker0/source/mips/include/unit.h" 2

extern ostream& operator<<(ostream&, unit);

//
// A unit is used in cases where the type signature of an overloaded
// function must be differentiated in some stronger way than can be
// denoted unambiguously in the C++ syntax.  This enum is used to give
// one of the functions a different type signature, thereby allowing
// the overloading.
//
// The use of ``unit'' instead of int or bool is important because a unit
// has only one possible value; therefore it adds no more information to
// the code.   For example, say a bool was used instead, then in the testing
// phase, would have to ask: what if TRUE was given, what if FALSE was given.
// The problem is compounded if char or int is used.
//
// Example:
//
//     class ID {
//     public:
//         ID();		// construct a null ID
//         ID(unit);		// alloc a ID with a new id
//     private:
//         static unsigned high_water;
//         unsigned id;
//     };
//
// Try working this example save that ID implements all of the generic
// features of the identifier object, but the high_water is stored
// in the heir.  That is what originally motivated the creation of unit.
//


//# 48 "/sandbox/wbaker/wbaker0/source/mips/include/stuff++.h" 2


//
// In the spirit of the standard GNU error handler functions
// as described in
//     typedef void (*one_arg_error_handler_t)(const char*);
//         a one argument error handler function pointer
//     typedef void (*two_arg_error_handler_t)(const char*, const char*);
//         a two argument error handler function pointer
//
// And now the NEW
//
// typedef void (*zero_arg_error_handler_t)();
//     a zero argument error handler function pointer
//
typedef void (*zero_arg_error_handler_t)();

//
// In the spirit of the default GNU error handler functions
// as described in
//     extern void default_one_arg_error_handler(const char *message);
//         print out message on stderr, and do the default thing (abort)
//     extern void default_two_arg_error_handler(const char *kind, const char *message);
//         print out kind and message on stderr, and do the default thing (abort)
//
// And now the NEW
//
// extern void default_zero_arg_error_handler(const char *message);
//     do the default thing (abort)
//
extern void default_zero_arg_error_handler();

// Guaranteed to exit (1)
extern void exit_zero_arg_error_handler();
extern void exit_one_arg_error_handler(const char *message);
extern void exit_two_arg_error_handler(const char *kind, const char *message);

// Guaranteed to abort()
extern void abort_zero_arg_error_handler();
extern void abort_one_arg_error_handler(const char *message);
extern void abort_two_arg_error_handler(const char *kind, const char *message);

//
// In the spirit of the standard GNU error handlers
// as described in
//     extern void  verbose_File_error_handler(const char*);
//         perror and set errno = 0
//     extern void  quiet_File_error_handler(const char*);
//         set errno = 0
//     extern void  fatal_File_error_handler(const char*);
//         perror and exit 1
//
// And now the NEW
//
// extern void preserve_File_error_handler(const char *message);
//     no perror, no assignment to errno.
//
extern void preserve_File_error_handler(const char *message);


//# 29 "../../../../mips/include/Gct/GctErrorHandler.h" 2

//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h" 1
// -*- C++ -*-



//
// tostrstream class
//
// A terminated oststream - an ostsrstream that auto-terminates on str()
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/strstream.h" 1
//    This is part of the iostream library, providing input/output for C++.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.




//#pragma interface

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 23 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/strstream.h" 2


class strstreambuf : public streambuf {
    size_t *lenp; /* current (logical) length (i.e. valid data bytes) */
    size_t *sizep; /* allocated (physical) buffer size */
    char **bufp;
    size_t _len;
    size_t _size;
    char *buf;
    int _frozen;
  protected:
    virtual int overflow(int = (-1) );
  public:
    strstreambuf();
    strstreambuf(int initial);
    strstreambuf(char *ptr, int size, char *pstart = 0 );
    ~strstreambuf();
    int frozen() { return _frozen; }
    void freeze(int n=1) { _frozen = n != 0; }
    size_t pcount();
    char *str();
};

class istrstream : public istream {
  public:
    istrstream(char*);
    istrstream(char*, int);
    strstreambuf* rdbuf() { return (strstreambuf*)_strbuf; }
};

class ostrstream : public ostream {
  public:
    ostrstream();
    ostrstream(char *cp, int n, int mode=ios::out);
    size_t pcount() { return ((strstreambuf*)_strbuf)->pcount(); }
    char *str() { return ((strstreambuf*)_strbuf)->str(); }
    void freeze(int n = 1) { ((strstreambuf*)_strbuf)->freeze(n); }
    int frozen() { return ((strstreambuf*)_strbuf)->frozen(); }
    strstreambuf* rdbuf() { return (strstreambuf*)_strbuf; }
};


//# 25 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h" 2


//
// tostrstream class
//
// An isteam class that doesn't have that nasty skipws parameter that
// you have to remember to set.  This class simply provides the istream
// functionality with a set of constructors which defaults skipws to
// FALSE (instead of defaulting to TRUE as is the case with plain istream).
//
class tostrstream: public ostrstream {
public:
    tostrstream(): ostrstream()
	{ }
    // This constructor defines cp as the buffer to use for the
    // stream (instead of one of its own devising); it does NOT
    // initialize the ostrstream to contain cp (of length n).
    tostrstream(char *cp, int n, int mode=ios::out): ostrstream(cp, n, mode)
	{ }
    char *str()
	{
	    char *s = ostrstream::str();
	    s[ostrstream::pcount()] = '\0';
	    return s;
	}
};


//# 30 "../../../../mips/include/Gct/GctErrorHandler.h" 2


//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttObject.h" 1
// -*- C++ -*-



//
// GttObject class (is abstract)
//
// Expects to be included where needed explicitly.
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h" 1
// -*- C++ -*-
//# 52 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h"

//# 25 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttObject.h" 2


//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttErrorHandler.h" 1
// -*- C++ -*-



//
// GttErrorHandler class
//
// Expects to be included by Gtt.h
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/bool.h" 1







//# 25 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttErrorHandler.h" 2


//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/stuff++.h" 1
// -*- C++ -*-
//# 107 "/sandbox/wbaker/wbaker0/source/mips/include/stuff++.h"

//# 27 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttErrorHandler.h" 2

//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h" 1
// -*- C++ -*-
//# 52 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h"

//# 28 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttErrorHandler.h" 2


//
// It is expected that this will be virtually multiply inherited
// into all of the classes that need error reporting services.
//
// The typical way to have that done is by inheriting the GttObject
// as a virtual base class.
//

//
// GttErrorHandler class
//
class GttErrorHandler {
public:
    GttErrorHandler();
    GttErrorHandler(const char *program);
    virtual ~GttErrorHandler();

    //
    // Error messages
    //     - an unacceptable, but expected and recoverable condition
    //       was detected (but the test fails)
    //     - errors are for ``the expected environment was not found''
    //       rather than for ``file couldn't be opened''
    //     - these messages cannot be shut off
    //     - the error handler determines the recovery action
    //       TODO - one day exceptions will be used here
    //
    static void error(const char *message);
    static void error(tostrstream& message);

    static void error(const char *function, const char *message);
    static void error(const char *function, tostrstream& message);

    static void error(const char *class_name, const char *method, const char *message);
    static void error(const char *class_name, const char *method, tostrstream& message);

    //
    // Fatal messages
    //     - an unacceptable and unexpected error was detected
    //       the data invariants were violated, there is no recovery
    //     - these messages cannot be shut off
    //     - the error handler determines the recovery action
    //       TODO - one day exceptions will be used here
    //
    static void fatal(const char *message);
    static void fatal(tostrstream& message);

    static void fatal(const char *function, const char *message);
    static void fatal(const char *function, tostrstream& message);

    static void fatal(const char *class_name, const char *method, const char *message);
    static void fatal(const char *class_name, const char *method, tostrstream& message);
private:
    //
    // Two underscores are used here in order to prevent confusion of these
    // private variables with any of the heir's private variables.  Note that
    // access control is different than visibility in C++, so all the variable
    // names in a class hierarchy must be unique.
    //

    static bool __partial_init;
    static void __partial_initialize();
    static bool __full_init;
    static void __full_initialize(const char *program);
    static char *__program;

    static void __handle_error();
    static void __handle_fatal();
    static void __add_newline(const char *message);

    static bool __output_valid();
    static ostream *__output;
};


//# 27 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttObject.h" 2


//
// GttObject class (is abstract)
//
class GttObject: virtual public GttErrorHandler {
protected:
    GttObject();
    GttObject(const GttObject&);
    virtual ~GttObject();	// ensure descendants have virtual destructors

public:
    //
    // I/O Support
    //
    // The value typically persists only long enough for an i/o operation
    // to be performed (see the defintion of output via operator<<(... ) below)
    virtual const char *stringify();
protected:
    // This is the buffer into which the printed representation of this
    // object will be put when the time comes.  It is associated with the
    // object so it will never go away (so long as the object exists).
    // Use a pointer so that you only pay for the space when I/O is used
    tostrstream *stringbuf;
    void clear_stringbuf();

public:
    //
    // Consistency
    //
    // The global data invariant for the whole object (heirs included).
    // This OK function will call the local invariant function ok() if
    // necessary and in addition the OK functions of the heirs
    // This is expected to compute the data invariant of the object.
    // It will execute GctErrorHandler::fatal if there is wrong.
    virtual void OK() const;

protected:
    //
    // consistency
    //
    // This function computes the invariant which is local to this object.
    // It does not call any of the ancestor's OK() or ok() functions.
    // It is not a virtual function so that it can be called from within a
    // constructor with impunity.  Thus this function MUST NOT call any
    // virtual functions either; it should call them by their full name if
    // that is necessary.  The global OK() function will call this function
    // as necessary.
    //
    // This function must NOT NEVER EVER be made virtual.
    void ok() const;

protected:
    //
    // Class Name
    //
    // This must return a static (constant) string which is the name
    // of the class being declared.  By convention, not all classes
    // must have one of these, but the major root abstract class must
    // have one in order to allow the stringify() to work approximately
    // correctly.
    virtual const char *class_name() const = 0;
};

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 91 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttObject.h" 2

extern ostream& operator<<(ostream&, GttObject&);

// There may be other X& operator<<(X&, GttObject&) defined in the
// packages defining class X.  For example see the definition of
// GttUnitObject& operator<<(GttUnitObject&, GttObject&) in Unit.


//# 32 "../../../../mips/include/Gct/GctErrorHandler.h" 2


//
// It is expected that this will be virtually multiply inherited
// into all of the classes that need error reporting services.
//
// The typical way to have that done is by inheriting the GctObject
// as a virtual base class.
//

//
// GctErrorHandler class
//
// GPP_1_96_BUG
// NOTE - virtual public GttObject should be MI into GctObject - but that
// causes g++ 1.96 to segfault; so we must inherit GttObject here and use SI
// GPP_1_96_BUG
class GctErrorHandler: virtual public GttObject {
public:
    GctErrorHandler();
    GctErrorHandler(const String& program);
    virtual ~GctErrorHandler();

    //
    // Debugging messages
    //     - these are turned off for production code.
    //     - these messages can be shut off
    //
    static void debug(const char *message);
    static void debug(tostrstream& message);

    static void debug(const char *function, const char *message);
    static void debug(const char *function, tostrstream& message);

    static void debug(const char *class_name, const char *method, const char *message);
    static void debug(const char *class_name, const char *method, tostrstream& message);

    static bool debug();		// current debug switch
    static void debug(bool value);	// change the debug switch

    //
    // Informational messages
    //     - these correspond to a ``verbose'' mode
    //     - these are not errors, just chatty progress reports
    //     - these messages can be shut off
    //
    static void note(const char *message);
    static void note(tostrstream& message);

    static void note(const char *function, const char *message);
    static void note(const char *function, tostrstream& message);

    static void note(const char *class_name, const char *method, const char *message);
    static void note(const char *class_name, const char *method, tostrstream& message);

    static bool note();			// current note switch
    static void note(bool value);	// change the note switch

    //
    // Warning messages
    //     - warnings are system-recoverable errors
    //     - the system has noticed something and taken some
    //       corrective action
    //     - these messages can be shut off
    //
    static void warning(const char *message);
    static void warning(tostrstream& message);

    static void warning(const char *function, const char *message);
    static void warning(const char *function, tostrstream& message);

    static void warning(const char *class_name, const char *method, const char *message);
    static void warning(const char *class_name, const char *method, tostrstream& message);

    static bool warning();		// current warning switch
    static void warning(bool value);	// change the warning switch

    //
    // Error messages
    //     - an unacceptable, but expected and recoverable
    //       condition was detected
    //     - errors are for ``the expected environment was not found''
    //       rather than for ``file couldn't be opened''
    //     - these messages cannot be shut off
    //     - the error handler determines the recovery action
    //       TODO - one day exceptions will be used here
    //
    static void error(const char *message);
    static void error(tostrstream& message);

    static void error(const char *function, const char *message);
    static void error(const char *function, tostrstream& message);

    static void error(const char *class_name, const char *method, const char *message);
    static void error(const char *class_name, const char *method, tostrstream& message);

    // can't turn off errors - no ``static void error(bool value);''
    static zero_arg_error_handler_t error();		// current error handler
    static void error(zero_arg_error_handler_t handler);// change the error handler

    static void error_is_lib_error_handler();		// change the error handler
    static void error_is_exit();			// change the error handler

    // Describes the fatal handler - WATCHOUT - implicitly uses AllocRing
    static const char *error_handler_description();

    //
    // Fatal messages
    //     - an unacceptable and unexpected error was detected
    //       the data invariants were violated, there is no recovery
    //     - these messages cannot be shut off
    //     - the error handler determines the recovery action
    //       TODO - one day exceptions will be used here
    //
    static void fatal(const char *message);
    static void fatal(tostrstream& message);

    static void fatal(const char *function, const char *message);
    static void fatal(const char *function, tostrstream& message);

    static void fatal(const char *class_name, const char *method, const char *message);
    static void fatal(const char *class_name, const char *method, tostrstream& message);

    // can't turn off fatals - no ``static void fatal(bool value);''
    static zero_arg_error_handler_t fatal();			// return the fatal handler
    static void fatal(zero_arg_error_handler_t handler);	// change the fatal handler

    static void fatal_is_exit();	// change the fatal handler
    static void fatal_is_abort();	// change the fatal handler

    // Describes the fatal handler - WATCHOUT - implicitly uses AllocRing
    static const char *fatal_handler_description();
private:
    //
    // Two underscores are used here in order to prevent confusion of these
    // private variables with any of the heir's private variables.  Note that
    // access control is different than visibility in C++, so all the variable
    // names in a class hierarchy must be unique.
    //
    static bool __debug;
    static bool __note;
    static bool __warning;
    static void (*__error_handler)();	// can't turn off errors
    static void (*__fatal_handler)();	// can't turn off fatals

    static bool __partial_init;
    static void __partial_initialize();
    static bool __full_init;
    static void __full_initialize(const char *program);
    static char *__program;

    static void __handle_error();
    static void __handle_fatal();
    static void __add_newline(const char *message);
    static void __message_switch(bool value, bool& flag, const char *description);
    static void __message_switch(bool value, bool& flag);
    static const char *__describe_handler(zero_arg_error_handler_t handler);

    static bool __output_valid();
    static ostream *__output;

    // GPP_1_96_BUG
    const char *class_name() const;
    // GPP_1_96_BUG
};


//# 27 "../../../../mips/include/Gct/Reference/GctReferenceCount.h" 2


class GctReferenceCount: virtual public GctErrorHandler {
public:
    GctReferenceCount();

    void inc();
    void dec();
    bool zero() const;
private:
    unsigned _count;
};

inline
GctReferenceCount::GctReferenceCount()
{
    this->_count = 0;
}

inline void
GctReferenceCount::inc()
{
    this->_count++;
}

inline void
GctReferenceCount::dec()
{
    if (this->debug()) {
	if (this->_count == 0)
	    this->error("GctReferenceCount", "dec()",
			"attempt to decrement a zero refcount");
    }

    // Protect against decrementing off zero in case
    // this->debug() is not turned on to signal the error.
    if (this->_count)
	this->_count--;
}

inline bool
GctReferenceCount::zero() const
{
    return (bool)(this->_count == 0);
}


//# 25 "../../../../mips/include/Gct/Reference.h" 2

//# 1 "../../../../mips/include/Gct/Reference/GctReferenceObject.h" 1
// -*- C++ -*-



//
// GctReferenceObject class
//
// Expects to be included by Gct/Reference.h
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/bool.h" 1







//# 25 "../../../../mips/include/Gct/Reference/GctReferenceObject.h" 2


//# 1 "../../../../mips/include/Gct/Object/GctHashObject.h" 1
// -*- C++ -*-



//
// GctHashObject class (is abstract)
//
// Expects to be included by Object.h or where needed explicitly.
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "../../../../mips/include/Gct/Object/GctObject.h" 1
// -*- C++ -*-



//
// GctObject class (is abstract)
//
// Expects to be included by Object.h or where needed explicitly.
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h" 1
// -*- C++ -*-
//# 52 "/sandbox/wbaker/wbaker0/source/mips/include/tostrstream.h"

//# 25 "../../../../mips/include/Gct/Object/GctObject.h" 2


//# 1 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttObject.h" 1
// -*- C++ -*-
//# 98 "/sandbox/wbaker/wbaker0/source/mips/include/Gtt/GttObject.h"

//# 27 "../../../../mips/include/Gct/Object/GctObject.h" 2


//# 1 "../../../../mips/include/Gct/GctErrorHandler.h" 1
// -*- C++ -*-
//# 198 "../../../../mips/include/Gct/GctErrorHandler.h"

//# 29 "../../../../mips/include/Gct/Object/GctObject.h" 2


//
// GctObject class (is abstract)
//
class GctObject: virtual public GctErrorHandler /*, virtual public GttObject*/ {
protected:
    GctObject();
    GctObject(const GctObject&);
    virtual ~GctObject();	// ensure descendants have virtual destructors

public:
    //
    // I/O Support
    //
    // The value typically persists only long enough for an i/o operation
    // to be performed (see the defintion of output via operator<<(... ) below)
    virtual const char *stringify();
protected:
    // This is the buffer into which the printed representation of this
    // object will be put when the time comes.  It is associated with the
    // object so it will never go away (so long as the object exists).
    // Use a pointer so that you only pay for the space when I/O is used
    tostrstream *stringbuf;
    void clear_stringbuf();

public:
    //
    // Consistency (global consistency)
    //
    // The global data invariant for the whole object (heirs included).
    // This OK function will call the local invariant function ok() if
    // necessary and in addition the OK functions of the heirs
    // This is expected to compute the data invariant of the object.
    // It will execute GctErrorHandler::fatal if there is wrong.
    virtual void OK() const;

protected:
    //
    // consistency (local consistency)
    //
    // This function computes the invariant which is local to this object.
    // It does not call any of the ancestor's OK() or ok() functions.
    // It is not a virtual function so that it can be called from within a
    // constructor with impunity.  Thus this function MUST NOT call any
    // virtual functions either; it should call them by their full name if
    // that is necessary.  The global OK() function will call this function
    // as necessary.
    //
    // This function must NOT NEVER EVER be made virtual.
    void ok() const;
protected:
    //
    // Class Name
    //
    // This must return a static (constant) string which is the name
    // of the class being declared.  By convention, not all classes
    // must have one of these, but the major root abstract class must
    // have one in order to allow the stringify() to work approximately
    // correctly.
    virtual const char *class_name() const = 0;

public:
    //
    // The ``id'' of this object
    //
    // NOTE - we explicitly allow the situation where this function
    // can return the address of the object - the ``this'' pointer
    // instead of a computed id field (the __object_id field below).
    //
    // This function is protected because we don't want too much dependence
    // on this notion of object identity.  I want to be able to rip it
    // out if it becomes to cumbersome.
    unsigned objectId() const;
private:
    //
    // Symbolic ID
    //
    // NOTE - Normally this would be implemented by the `this' pointer.
    // TODO - remove this for production code
    //
    // However, in order to make the test suites run on all machines, we
    // make this into a symbolic id that is maintained with each object.
    // Thus the valid outputs are always consistent across all machines.
    unsigned __object_id;
    static unsigned __next_id;
};

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 117 "../../../../mips/include/Gct/Object/GctObject.h" 2

extern ostream& operator<<(ostream&, GctObject&);


//# 25 "../../../../mips/include/Gct/Object/GctHashObject.h" 2


//
// GctHashObject class (is abstract)
//
class GctHashObject: virtual public GctObject {
protected:
    GctHashObject();
    GctHashObject(const GctHashObject&);

public:
    //
    // hash support
    //
    virtual unsigned hash() const;
};


//# 27 "../../../../mips/include/Gct/Reference/GctReferenceObject.h" 2


class GctRef;

//
// GctReferenceObject class
//
class GctReferenceObject: virtual public GctHashObject {
public:
    // Aid in using operator new() to allocate dynamic
    // instances of heirs to GctReferenceObject.
    // An error is reported if result_of_new is nil.
    static void ensure_new_succeeded(GctReferenceObject *result_of_new,
				     const char *class_name,
				     const char *args_supplied = "");

    // the stringify() is standard
    void OK() const;
protected:
    GctReferenceObject(const bool refcounting = FALSE);
    GctReferenceObject(const GctReferenceObject&,
		       const bool refcounting = FALSE);
    virtual ~GctReferenceObject();

    void operator=(const GctReferenceObject&);

    void ok() const;

    // Disallowed (create pointers to objects via new only)
    // It is kept protected so that heirs can do *-cast-& tricks to work
    // around the contravariance of abstract virtuals.  This operator should
    // never be used to return an address of a GctReferenceObject.
    GctReferenceObject *operator&() const;
private:
    const char *class_name() const;

    bool _destructed;	// ensure no use of destructed objects
    // this bit guards against the destruct-before-copy g++ bug

    bool _refcounting;	// was allocated via operator new()
    GctReferenceCount _refcount;

    // TODO - these names can never be used again in a derived
    // class; should they be obfuscated some to free up the
    // names (these are good, commonly-used names).
    void reference();
    void dereference(GctReferenceObject *&);
    void kill();

    bool refcounted() const;
    void refcounting();	// as if given in the constructor

    // Using this scheme, only GctRef can manipulate the
    // reference count via reference(), dereference().  The derived
    // classes cannot do such, and the _refcount field is not
    // available to be tweaked by GctRef in any way other
    // way than the inc/dec/zero interface.
    friend class GctRef;
};


//# 26 "../../../../mips/include/Gct/Reference.h" 2

//# 1 "../../../../mips/include/Gct/Reference/GctRef.h" 1
// -*- C++ -*-



//
// GctRef class
//
// Expects to be included by Gct/Reference.h
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/bool.h" 1







//# 25 "../../../../mips/include/Gct/Reference/GctRef.h" 2


//# 1 "../../../../mips/include/Gct/Object/GctHashObject.h" 1
// -*- C++ -*-
//# 42 "../../../../mips/include/Gct/Object/GctHashObject.h"

//# 27 "../../../../mips/include/Gct/Reference/GctRef.h" 2


class GctReferenceObject;

class GctRef: virtual public GctHashObject {
public:
    GctRef();
    GctRef(const GctRef&);
    virtual ~GctRef();

    void operator=(GctRef&);

    // Accessor functions
    bool valid() const;			// the managed pointer is !null
    bool null() const;			// the managed pointer is null

    GctReferenceObject *ref() const;	// faults if not valid
    operator void*() const;		// unchecked reference

    void nullify();			// set the managed pointer to null
    void invalidate();			// set the managed pointer to null

    unsigned hash() const;

    // the stringify() is standard
    void OK() const;
protected:
    void ok() const;

    // We want to make sure pointers do not float around for very long.
    // This constructor is used by descendants of this class only; it is
    // expected that such descendants will have friend functions which
    // will call ``new MumbleObject()'' and return a GctRef to THAT.
    // MumbleObject would be a descendant of GctReferenceObject
    GctRef(GctReferenceObject *);
    friend class GctReferenceObject;
private:
    const char *class_name() const;

    GctReferenceObject *_pointer;	// may store 0

    bool _destructed;	// ensure no use of destructed objects
    // this bit guards against the destruct-before-copy g++ bug
};

inline
GctRef::operator void*() const
{ return this->_pointer; }

inline bool
GctRef::valid() const
{ return (bool)(this->_pointer != 0); }

inline bool
GctRef::null() const
{ return (bool)(this->_pointer == 0); }

//
// Required operations for the container classes
//
// NOTE: we can't use const here because the genclass code doesn't
// That is the ONLY reason for not using const reference args here.
//
extern int operator==(GctRef&, GctRef&);
extern int operator!=(GctRef&, GctRef&);

extern int operator<=(GctRef&, GctRef&);
extern int operator<(GctRef&, GctRef&);

extern int operator>=(GctRef&, GctRef&);
extern int operator>(GctRef&, GctRef&);

extern unsigned hash(GctRef&);


//# 27 "../../../../mips/include/Gct/Reference.h" 2



//# 47 "../../../../mips/include/Gct/genclasses.h" 2


//# 1 "../../../../mips/include/Gct/Ast/GctAstRef.h" 1
// -*- C++ -*-



//
// GctAstRef class
//
// Expects to be included by Gct/Ast.h
//
// Wendell Baker, Berkeley CAD Group, 1991 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "../../../../mips/include/Gct/Reference.h" 1
// -*- C++ -*-
//# 29 "../../../../mips/include/Gct/Reference.h"

//# 25 "../../../../mips/include/Gct/Ast/GctAstRef.h" 2


//# 1 "../../../../mips/include/Gct/Ast/GctAstType.h" 1
// -*- C++ -*-



//
// GctAstType enum
//
// Wendell Baker, Berkeley CAD Group, 1992 (wbaker@ic.Berkeley.EDU)
//


//
// GctAstType enum
//
enum GctAstType {
    Gct_Leaf,
    Gct_Tree
};

extern const char *stringify(GctAstType a);

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 26 "../../../../mips/include/Gct/Ast/GctAstType.h" 2

extern ostream& operator<<(ostream&, GctAstType);


//# 27 "../../../../mips/include/Gct/Ast/GctAstRef.h" 2


class GctAst;
class GctTree;
class GctLeaf;

class GctAstRef: public GctRef {
public:
    GctAstRef();
    GctAstRef(const GctAstRef&);

    GctAst *ref() const;		// may return 0 (not requires valid)

    GctAstType concrete_type() const;	// requires valid()
    GctTree *tree() const;		// never return 0 - typechecked
    GctLeaf *leaf() const;		// never return 0 - typechecked
protected:
    // We make sure pointers do not float around for very long.
    // This constructor cooperates with Gct{Leaf,Tree}::New
    // (all flavors) to allow for the creation of new dynamic structures.
    GctAstRef(GctAst *);
    friend class GctLeaf;
    friend class GctTree;
};


//# 49 "../../../../mips/include/Gct/genclasses.h" 2



//# 2 "GctNameRef.List.cc" 2

//# 1 "../../../../mips/include/Gct/Name/GctNameRef.h" 1
// -*- C++ -*-



//
// GctNameRef class
//
// Expects to be included by Gct/Name.h
//
// Wendell Baker, Berkeley CAD Group, 1991 (wbaker@ic.Berkeley.EDU)
//





//#pragma interface



//# 1 "../../../../mips/include/Gct/Reference.h" 1
// -*- C++ -*-
//# 29 "../../../../mips/include/Gct/Reference.h"

//# 25 "../../../../mips/include/Gct/Name/GctNameRef.h" 2


//# 1 "../../../../mips/include/Gct/Name/GctNameType.h" 1
// -*- C++ -*-



//
// GctNameType enum
//
// Wendell Baker, Berkeley CAD Group, 1991 (wbaker@ic.Berkeley.EDU)
//


//
// GctNameType enum
//
enum GctNameType {
    Gct_Identifier,
    Gct_Signature,
};

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h" 1
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//# 210 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/iostream.h"

//# 24 "../../../../mips/include/Gct/Name/GctNameType.h" 2


extern char *ntoa(GctNameType n, unsigned width = 0);
extern ostream& operator<<(ostream&, GctNameType);


//# 27 "../../../../mips/include/Gct/Name/GctNameRef.h" 2


class GctName;
class GctIdentifier;
class GctSignature;

class GctNameRef: public GctRef {
public:
    GctNameRef();
    GctNameRef(const GctNameRef&);

    GctName *ref() const;		// may return 0 (not requires valid())

    GctNameType concrete_type() const;	// requires valid()
    GctIdentifier *identifier() const;	// never return 0 - typechecked
    GctSignature *signature() const;	// never return 0 - typechecked
protected:
    // We make sure pointers do not float around for very long.
    // This constructor cooperates with GctIdentifier::New and
    // GctSignature::New  (all flavors) to allow for the creation of new
    // dynamic structures.
    GctNameRef(GctName *);
    friend class GctIdentifier;
    friend class GctSignature;

private:
    const char *class_name() const;
};

//
// Required operations for use by genclass container classes
//
// These are more specific than the ones supplied by GctRef
// because they use the name of the attribute as the key for
// comparison instead of the pointer stored as a void* value.
//
// NOTE: we can't use const here because the genclass code doesn't
// That is the ONLY reason for not using const reference args here.
//
extern int operator==(GctNameRef&, GctNameRef&);
extern int operator!=(GctNameRef&, GctNameRef&);

extern int operator<=(GctNameRef&, GctNameRef&);
extern int operator<(GctNameRef&, GctNameRef&);

extern int operator>=(GctNameRef&, GctNameRef&);
extern int operator>(GctNameRef&, GctNameRef&);

// extern unsigned hash(GctNameRef&)
// is handled by the previous declaration of
// extern unsigned hash(GctRef&);


//# 3 "GctNameRef.List.cc" 2

// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.
*/


//#pragma implementation

//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/builtin.h" 1
// This may look like C code, but it is really -*- C++ -*-

/*
Copyright (C) 1988, 1992 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
  arithmetic, etc. functions on built in types
*/


//# 214 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/builtin.h"

//# 30 "GctNameRef.List.cc" 2

//# 1 "genclasses/GctNameRef.List.h" 1
//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/Pix.h" 1



typedef void* Pix;

//# 1 "genclasses/GctNameRef.List.h" 2

// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.
*/




//#pragma once
//#pragma interface





typedef void (*GctNameRefProcedure)(GctNameRef&);
typedef GctNameRef  (*GctNameRefMapper)(GctNameRef&);
typedef GctNameRef& (*GctNameRefCombiner)(const GctNameRef&, 
					  const GctNameRef&);
typedef int  (*GctNameRefPredicate)(GctNameRef&);
typedef int  (*GctNameRefComparator)(GctNameRef&, GctNameRef&);


//# 1 "/projects/gnu-cygnus/gnu-cygnus-8/common/g++-include/Pix.h" 1





//# 42 "genclasses/GctNameRef.List.h" 2


struct GctNameRefListNode
{
  GctNameRefListNode*          tl;
  short                 ref;
  GctNameRef                   hd;
};

extern GctNameRefListNode NilGctNameRefListNode;

class GctNameRefList
{
protected:
  GctNameRefListNode*          P;

                        GctNameRefList(GctNameRefListNode* p);
public:
                        GctNameRefList();
                        GctNameRefList(GctNameRef& head);
                        GctNameRefList(GctNameRef& head, GctNameRefList& tl);
                        GctNameRefList(const GctNameRefList& a);
                        GctNameRefList(Pix p);
                        ~GctNameRefList();

  GctNameRefList&              operator = (const GctNameRefList& a);

  int                   null();
  int                   valid();
                        operator const void* ();
  int                   operator ! ();

  int                   length() const;
  int                   list_length();

  GctNameRef&                  get();
  GctNameRef&                  head();
  GctNameRef&                  operator [] (int n);

  GctNameRefList               nth(int n);
  GctNameRefList               tail();
  GctNameRefList               last() const;

  GctNameRefList               find(GctNameRef& targ);
  GctNameRefList               find(GctNameRefList& targ);
  int                   contains(GctNameRef& targ) const;
  int                   contains(GctNameRefList& targ) const;
  int                   position(GctNameRef& targ);

  friend GctNameRefList        copy(GctNameRefList& a);
  friend GctNameRefList        concat(GctNameRefList& a, GctNameRefList& b);
  friend GctNameRefList        append(GctNameRefList& a, GctNameRefList& b);
  friend GctNameRefList        map(GctNameRefMapper f, GctNameRefList& a);
  friend GctNameRefList        merge(GctNameRefList& a, GctNameRefList& b, GctNameRefComparator f);
  friend GctNameRefList        combine(GctNameRefCombiner f, GctNameRefList& a, GctNameRefList& b);
  friend GctNameRefList        reverse(GctNameRefList& a);
  friend GctNameRefList        select(GctNameRefPredicate f, GctNameRefList& a);
  friend GctNameRefList        remove(GctNameRef& targ, GctNameRefList& a);
  friend GctNameRefList        remove(GctNameRefPredicate f, GctNameRefList& a);
  friend GctNameRefList        subst(GctNameRef& old, GctNameRef& repl, GctNameRefList& a);

  void                  push(GctNameRef& x);
  GctNameRef                   pop();

  void                  set_tail(GctNameRefList& p);
  void                  append(GctNameRefList& p);
  void                  prepend(GctNameRefList& p);
  void                  del(GctNameRef& targ);
  void                  del(GctNameRefPredicate f);
  void                  select(GctNameRefPredicate f);
  void                  subst(GctNameRef& old, GctNameRef& repl);
  void                  reverse();
  void                  sort(GctNameRefComparator f);

  void                  apply(GctNameRefProcedure f);
  GctNameRef                   reduce(GctNameRefCombiner f, GctNameRef& base);

  friend int            operator == (GctNameRefList& a, GctNameRefList& b);
  friend int            operator != (GctNameRefList& a, GctNameRefList& b);

  Pix                   first() const;
  void                  next(Pix& p) const;
  Pix                   seek(GctNameRef& item) const;
  GctNameRef&                  operator () (Pix p) const;
  int                   owns(Pix p) const;

  void                  error(const char*) const;
  int                   OK() const;
};



inline void reference(GctNameRefListNode* p)
{
  if (p->ref >= 0) ++p->ref;
}

inline void dereference(GctNameRefListNode* p)
{
  while (p->ref > 0 && --p->ref == 0)
  {
    GctNameRefListNode* n = p->tl;
    delete(p);
    p = n;
  }
}


inline GctNameRefListNode* newGctNameRefListNode(const GctNameRef& h)
{
  GctNameRefListNode* p = new GctNameRefListNode;
  p->ref = 1;
  p->hd = (GctNameRef&) h;
  return p;
}

inline GctNameRefListNode* newGctNameRefListNode(GctNameRef& h, GctNameRefListNode* t)
{
  GctNameRefListNode* p = new GctNameRefListNode;
  p->ref = 1;
  p->hd = h;
  p->tl = t;
  return p;
}


inline GctNameRefList::~GctNameRefList()
{
  dereference(P);
}

inline GctNameRefList::GctNameRefList()
{
  P = &NilGctNameRefListNode;
}

inline GctNameRefList::GctNameRefList(GctNameRefListNode* p)
{
  P = p;
}

inline GctNameRefList::GctNameRefList(GctNameRef& head)
{
  P = newGctNameRefListNode(head);
  P->tl = &NilGctNameRefListNode;
}

inline GctNameRefList::GctNameRefList(GctNameRef& head, GctNameRefList& tl)
{
  P = newGctNameRefListNode(head, tl.P);
  reference(P->tl);
}

inline GctNameRefList::GctNameRefList(const GctNameRefList& a)
{
  GctNameRefList& gl = (GctNameRefList&) a;
  reference(gl.P);
  P = a.P;
}


inline GctNameRef& GctNameRefList::get()
{
  return P->hd;
}

inline GctNameRef& GctNameRefList::head()
{
  return P->hd;
}


inline GctNameRefList GctNameRefList::tail()
{
  reference(P->tl);
  return GctNameRefList(P->tl);
}



inline int GctNameRefList::null()
{
  return P == &NilGctNameRefListNode;
}

inline int GctNameRefList::valid()
{
  return P != &NilGctNameRefListNode;
}

inline GctNameRefList::operator const void* ()
{
  return (P == &NilGctNameRefListNode)? 0 : this;
}

inline int GctNameRefList::operator ! ()
{
  return (P == &NilGctNameRefListNode);
}


inline void GctNameRefList::push(GctNameRef& head)
{
  GctNameRefListNode* oldp = P;
  P = newGctNameRefListNode(head, oldp);
}


inline int operator != (GctNameRefList& x, GctNameRefList& y)
{
  return !(x == y);
}

inline Pix GctNameRefList::first() const
{
  return (P == &NilGctNameRefListNode)? 0 : Pix(P);
}

inline GctNameRef& GctNameRefList::operator () (Pix p) const
{
  return ((GctNameRefListNode*)p)->hd;
}

inline void GctNameRefList::next(Pix& p) const
{
  if (p != 0)
  {
    p = Pix(((GctNameRefListNode*)p)->tl);
    if (p == &NilGctNameRefListNode) p = 0;
  }
}

inline GctNameRefList::GctNameRefList(Pix p)
{
  P = (GctNameRefListNode*)p;
  reference(P);
}



//# 31 "GctNameRef.List.cc" 2


GctNameRefListNode NilGctNameRefListNode;

class init_NilGctNameRefListNode
{
public:
  inline init_NilGctNameRefListNode()
  {
    NilGctNameRefListNode.tl = &NilGctNameRefListNode;
    NilGctNameRefListNode.ref = -1;
  }
};

static init_NilGctNameRefListNode NilGctNameRefListNode_initializer;

GctNameRefList& GctNameRefList::operator = (const GctNameRefList& a)
{
  reference(a.P);
  dereference(P);
  P = a.P;
  return *this;
}

GctNameRef GctNameRefList::pop()
{
  GctNameRef res = P->hd;
  GctNameRefListNode* tail = P->tl;
  reference(tail);
  dereference(P);
  P = tail;
  return res;
}

void GctNameRefList::set_tail(GctNameRefList& a)
{
  reference(a.P);
  dereference(P->tl);
  P->tl = a.P;
}

GctNameRefList GctNameRefList::nth(int n)
{
  GctNameRefListNode* p;
  for (p = P; n-- > 0; p = p->tl);
  reference(p);
  return GctNameRefList(p);
}

GctNameRefList GctNameRefList::last() const
{
  GctNameRefListNode* p = P;
  if (p != &NilGctNameRefListNode) while (p->tl != &NilGctNameRefListNode) p = p->tl;
  reference(p);
  return GctNameRefList(p);
}

void GctNameRefList::append(GctNameRefList& l)
{
  GctNameRefListNode* p = P;
  GctNameRefListNode* a = l.P;
  reference(a);
  if (p != &NilGctNameRefListNode)
  {
    while (p->tl != &NilGctNameRefListNode) p = p->tl;
    p->tl = a;
  }
  else
    P = a;
}

int GctNameRefList::length() const
{
  int l = 0;
  for (GctNameRefListNode* p = P; p != &NilGctNameRefListNode; p = p->tl) ++l;
  return l;
}

GctNameRef&  GctNameRefList::operator [] (int n)
{
  GctNameRefListNode* p;
  for (p = P; n-- > 0; p = p->tl);
  return (p->hd);
}

int operator == (GctNameRefList& x, GctNameRefList& y)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* b = y.P;

  for (;;)
  {
    if (a == &NilGctNameRefListNode)
      return b == &NilGctNameRefListNode;
    else if (b == &NilGctNameRefListNode)
      return 0;
    else if (a->hd == b->hd)
    {
      a = a->tl;
      b = b->tl;
    }
    else
      return 0;
  }
}


void GctNameRefList::apply(GctNameRefProcedure f)
{
  for(GctNameRefListNode* p = P; p != &NilGctNameRefListNode; p = p->tl)
    (*f)((p->hd));
}

void GctNameRefList::subst(GctNameRef& old, GctNameRef& repl)
{
  for(GctNameRefListNode* p = P; p != &NilGctNameRefListNode; p = p->tl)
    if (p->hd == old)
      p->hd = repl;
}

GctNameRef GctNameRefList::reduce(GctNameRefCombiner f, GctNameRef& base)
{
  GctNameRef r = base;
  for(GctNameRefListNode* p = P; p != &NilGctNameRefListNode; p = p->tl)
    r = (*f)(r, (p->hd));
  return r;
}

int GctNameRefList::position(GctNameRef& targ)
{
  int l = 0;
  GctNameRefListNode* p = P;
  for (;;)
  {
    if (p == &NilGctNameRefListNode)
      return -1;
    else if (p->hd == targ)
      return l;
    else
    {
      ++l;
      p = p->tl;
    }
  }
}

int GctNameRefList::contains(GctNameRef& targ) const
{
  GctNameRefListNode* p = P;
  for (;;)
  {
    if (p == &NilGctNameRefListNode)
      return 0;
    else if (p->hd == targ)
      return 1;
    else
      p = p->tl;
  }
}

GctNameRefList GctNameRefList::find(GctNameRef& targ)
{
  GctNameRefListNode* p;
  for (p = P; p != &NilGctNameRefListNode && !(p->hd == targ); p=p->tl);
  reference(p);
  return GctNameRefList(p);
}

Pix GctNameRefList::seek(GctNameRef& targ) const
{
  GctNameRefListNode* p = P;
  for (;;)
  {
    if (p == &NilGctNameRefListNode)
      return 0;
    else if (p->hd == targ)
      return Pix(p);
    else
      p = p->tl;
  }
}

int GctNameRefList::owns(Pix i) const
{
  GctNameRefListNode* p = P;
  for (;;)
  {
    if (p == &NilGctNameRefListNode)
      return 0;
    else if (Pix(p) == i)
      return 1;
    else
      p = p->tl;
  }
}

GctNameRefList GctNameRefList::find(GctNameRefList& target)
{
  GctNameRefListNode* targ = target.P;
  if (targ == &NilGctNameRefListNode)
    return GctNameRefList(targ);

  GctNameRefListNode* p = P;
  while (p != &NilGctNameRefListNode)
  {
    if (p->hd == targ->hd)
    {
      GctNameRefListNode* a = p->tl;
      GctNameRefListNode* t = targ->tl;
      for(;;)
      {
        if (t == &NilGctNameRefListNode)
        {
          reference(p);
          return GctNameRefList(p);
        }
        else if (a == &NilGctNameRefListNode || !(a->hd == t->hd))
          break;
        else
        {
          a = a->tl;
          t = t->tl;
        }
      }
    }
    p = p->tl;
  }
  return GctNameRefList(&NilGctNameRefListNode);
}

int GctNameRefList::contains(GctNameRefList& target) const
{
  GctNameRefListNode* targ = target.P;
  if (targ == &NilGctNameRefListNode)
    return 0;

  GctNameRefListNode* p = P;
  while (p != &NilGctNameRefListNode)
  {
    if (p->hd == targ->hd)
    {
      GctNameRefListNode* a = p->tl;
      GctNameRefListNode* t = targ->tl;
      for(;;)
      {
        if (t == &NilGctNameRefListNode)
          return 1;
        else if (a == &NilGctNameRefListNode || !(a->hd == t->hd))
          break;
        else
        {
          a = a->tl;
          t = t->tl;
        }
      }
    }
    p = p->tl;
  }
  return 0;
}

void GctNameRefList::del(GctNameRef& targ)
{
  GctNameRefListNode* h = P;

  for (;;)
  {
    if (h == &NilGctNameRefListNode)
    {
      P = h;
      return;
    }
    else if (h->hd == targ)
    {
      GctNameRefListNode* nxt = h->tl;
      reference(nxt);
      dereference(h);
      h = nxt;
    }
    else
      break;
  }

  GctNameRefListNode* trail = h;
  GctNameRefListNode* p = h->tl;
  while (p != &NilGctNameRefListNode)
  {
    if (p->hd == targ)
    {
      GctNameRefListNode* nxt = p->tl;
      reference(nxt);
      dereference(p);
      trail->tl = nxt;
      p = nxt;
    }
    else
    {
      trail = p;
      p = p->tl;
    }
  }
  P = h;
}

void GctNameRefList::del(GctNameRefPredicate f)
{
  GctNameRefListNode* h = P;
  for (;;)
  {
    if (h == &NilGctNameRefListNode)
    {
      P = h;
      return;
    }
    else if ((*f)(h->hd))
    {
      GctNameRefListNode* nxt = h->tl;
      reference(nxt);
      dereference(h);
      h = nxt;
    }
    else
      break;
  }

  GctNameRefListNode* trail = h;
  GctNameRefListNode* p = h->tl;
  while (p != &NilGctNameRefListNode)
  {
    if ((*f)(p->hd))
    {
      GctNameRefListNode* nxt = p->tl;
      reference(nxt);
      dereference(p);
      trail->tl = nxt;
      p = nxt;
    }
    else
    {
      trail = p;
      p = p->tl;
    }
  }
  P = h;
}

void GctNameRefList::select(GctNameRefPredicate f)
{
  GctNameRefListNode* h = P;
  for (;;)
  {
    if (h == &NilGctNameRefListNode)
    {
      P = h;
      return;
    }
    else if (!(*f)(h->hd))
    {
      GctNameRefListNode* nxt = h->tl;
      reference(nxt);
      dereference(h);
      h = nxt;
    }
    else
      break;
  }
  GctNameRefListNode* trail = h;
  GctNameRefListNode* p = h->tl;
  while (p != &NilGctNameRefListNode)
  {
    if (!(*f)(p->hd))
    {
      GctNameRefListNode* nxt = p->tl;
      reference(nxt);
      dereference(p);
      trail->tl = nxt;
      p = nxt;
    }
    else
    {
      trail = p;
      p = p->tl;
    }
  }
  P = h;
}

void GctNameRefList::reverse()
{
  GctNameRefListNode* l = &NilGctNameRefListNode;
  GctNameRefListNode* p = P;
  while (p != &NilGctNameRefListNode)
  {
    GctNameRefListNode* nxt = p->tl;
    p->tl = l;
    l = p;
    p = nxt;
  }
  P = l;
}


GctNameRefList copy(GctNameRefList& x)
{
  GctNameRefListNode* a = x.P;
  if (a == &NilGctNameRefListNode)
    return GctNameRefList(a);
  else
  {
    GctNameRefListNode* h = newGctNameRefListNode(a->hd);
    GctNameRefListNode* trail = h;
    for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode(a->hd);
      trail->tl = n;
      trail = n;
    }
    trail->tl = &NilGctNameRefListNode;
    return GctNameRefList(h);
  }
}


GctNameRefList subst(GctNameRef& old, GctNameRef& repl, GctNameRefList& x)
{
  GctNameRefListNode* a = x.P;
  if (a == &NilGctNameRefListNode)
    return GctNameRefList(a);
  else
  {
    GctNameRefListNode* h = new GctNameRefListNode;
    h->ref = 1;
    if (a->hd == old)
      h->hd = repl;
    else
      h->hd = a->hd;
    GctNameRefListNode* trail = h;
    for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
    {
      GctNameRefListNode* n = new GctNameRefListNode;
      n->ref = 1;
      if (a->hd == old)
        n->hd = repl;
      else
        n->hd = a->hd;
      trail->tl = n;
      trail = n;
    }
    trail->tl = &NilGctNameRefListNode;
    return GctNameRefList(h);
  }
}

GctNameRefList combine(GctNameRefCombiner f, GctNameRefList& x, GctNameRefList& y)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* b = y.P;
  if (a == &NilGctNameRefListNode || b == &NilGctNameRefListNode)
    return GctNameRefList(&NilGctNameRefListNode);
  else
  {
    GctNameRefListNode* h = newGctNameRefListNode((*f)(a->hd, b->hd));
    GctNameRefListNode* trail = h;
    a = a->tl;
    b = b->tl;
    while (a != &NilGctNameRefListNode && b != &NilGctNameRefListNode)
    {
      GctNameRefListNode* n = newGctNameRefListNode((*f)(a->hd, b->hd));
      trail->tl = n;
      trail = n;
      a = a->tl;
      b = b->tl;
    }
    trail->tl = &NilGctNameRefListNode;
    return GctNameRefList(h);
  }
}

GctNameRefList reverse(GctNameRefList& x)
{
  GctNameRefListNode* a = x.P;
  if (a == &NilGctNameRefListNode)
    return GctNameRefList(a);
  else
  {
    GctNameRefListNode* l = newGctNameRefListNode(a->hd);
    l->tl = &NilGctNameRefListNode;
    for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode(a->hd);
      n->tl = l;
      l = n;
    }
    return GctNameRefList(l);
  }
}

GctNameRefList append(GctNameRefList& x, GctNameRefList& y)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* b = y.P;
  reference(b);
  if (a != &NilGctNameRefListNode)
  {
    GctNameRefListNode* h = newGctNameRefListNode(a->hd);
    GctNameRefListNode* trail = h;
    for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode(a->hd);
      trail->tl = n;
      trail = n;
    }
    trail->tl = b;
    return GctNameRefList(h);
  }
  else
    return GctNameRefList(b);
}

void GctNameRefList::prepend(GctNameRefList& y)
{
  GctNameRefListNode* b = y.P;
  if (b != &NilGctNameRefListNode)
  {
    GctNameRefListNode* h = newGctNameRefListNode(b->hd);
    GctNameRefListNode* trail = h;
    for(b = b->tl; b != &NilGctNameRefListNode; b = b->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode(b->hd);
      trail->tl = n;
      trail = n;
    }
    trail->tl = P;
    P = h;
  }
}

GctNameRefList concat(GctNameRefList& x, GctNameRefList& y)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* b = y.P;
  if (a != &NilGctNameRefListNode)
  {
    GctNameRefListNode* h = newGctNameRefListNode(a->hd);
    GctNameRefListNode* trail = h;
    for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode(a->hd);
      trail->tl = n;
      trail = n;
    };
    for(;b != &NilGctNameRefListNode; b = b->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode(b->hd);
      trail->tl = n;
      trail = n;
    }
    trail->tl = &NilGctNameRefListNode;
    return GctNameRefList(h);
  }
  else if (b != &NilGctNameRefListNode)
  {
    GctNameRefListNode* h = newGctNameRefListNode(b->hd);
    GctNameRefListNode* trail = h;
    for(b = b->tl; b != &NilGctNameRefListNode; b = b->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode(b->hd);
      trail->tl = n;
      trail = n;
    }
    trail->tl = &NilGctNameRefListNode;
    return GctNameRefList(h);
  }
  else
    return GctNameRefList(&NilGctNameRefListNode);
}

GctNameRefList select(GctNameRefPredicate f, GctNameRefList& x)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* h = &NilGctNameRefListNode;
  while (a != &NilGctNameRefListNode)
  {
    if ((*f)(a->hd))
    {
      h = newGctNameRefListNode(a->hd);
      GctNameRefListNode* trail = h;
      for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
      {
        if ((*f)(a->hd))
        {
          GctNameRefListNode* n = newGctNameRefListNode(a->hd);
          trail->tl = n;
          trail = n;
        }
      }
      trail->tl = &NilGctNameRefListNode;
      break;
    }
    else
      a = a->tl;
  }
  return GctNameRefList(h);
}

GctNameRefList remove(GctNameRefPredicate f, GctNameRefList& x)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* h = &NilGctNameRefListNode;
  while (a != &NilGctNameRefListNode)
  {
    if (!(*f)(a->hd))
    {
      h = newGctNameRefListNode(a->hd);
      GctNameRefListNode* trail = h;
      for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
      {
        if (!(*f)(a->hd))
        {
          GctNameRefListNode* n = newGctNameRefListNode(a->hd);
          trail->tl = n;
          trail = n;
        }
      }
      trail->tl = &NilGctNameRefListNode;
      break;
    }
    else
      a = a->tl;
  }
  return GctNameRefList(h);
}

GctNameRefList remove(GctNameRef& targ, GctNameRefList& x)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* h = &NilGctNameRefListNode;
  while (a != &NilGctNameRefListNode)
  {
    if (!(a->hd == targ))
    {
      h = newGctNameRefListNode(a->hd);
      GctNameRefListNode* trail = h;
      for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
      {
        if (!(a->hd == targ))
        {
          GctNameRefListNode* n = newGctNameRefListNode(a->hd);
          trail->tl = n;
          trail = n;
        }
      }
      trail->tl = &NilGctNameRefListNode;
      break;
    }
    else
      a = a->tl;
  }
  return GctNameRefList(h);
}

GctNameRefList map(GctNameRefMapper f, GctNameRefList& x)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* h = &NilGctNameRefListNode;
  if (a != &NilGctNameRefListNode)
  {
    h = newGctNameRefListNode((*f)(a->hd));
    GctNameRefListNode* trail = h;
    for(a = a->tl; a != &NilGctNameRefListNode; a = a->tl)
    {
      GctNameRefListNode* n = newGctNameRefListNode((*f)(a->hd));
      trail->tl = n;
      trail = n;
    }
    trail->tl = &NilGctNameRefListNode;
  }
  return GctNameRefList(h);
}


GctNameRefList merge(GctNameRefList& x, GctNameRefList& y, GctNameRefComparator f)
{
  GctNameRefListNode* a = x.P;
  GctNameRefListNode* b = y.P;

  if (a == &NilGctNameRefListNode)
  {
    if (b == &NilGctNameRefListNode)
      return GctNameRefList(&NilGctNameRefListNode);
    else
      return copy(y);
  }
  else if (b == &NilGctNameRefListNode)
    return copy(x);

  GctNameRefListNode* h = new GctNameRefListNode;
  h->ref = 1;
  if ((*f)(a->hd, b->hd) <= 0)
  {
    h->hd = a->hd;
    a = a->tl;
  }
  else
  {
    h->hd = b->hd;
    b = b->tl;
  }

  GctNameRefListNode* r = h;

  for(;;)
  {
    if (a == &NilGctNameRefListNode)
    {
      while (b != &NilGctNameRefListNode)
      {
        GctNameRefListNode* n = new GctNameRefListNode;
        n->ref = 1;
        n->hd = b->hd;
        r->tl = n;
        r = n;
        b = b->tl;
      }
      r->tl = &NilGctNameRefListNode;
      return GctNameRefList(h);
    }
    else if (b == &NilGctNameRefListNode)
    {
      while (a != &NilGctNameRefListNode)
      {
        GctNameRefListNode* n = new GctNameRefListNode;
        n->ref = 1;
        n->hd = a->hd;
        r->tl = n;
        r = n;
        a = a->tl;
      }
      r->tl = &NilGctNameRefListNode;
      return GctNameRefList(h);
    }
    else if ((*f)(a->hd, b->hd) <= 0)
    {
      GctNameRefListNode* n = new GctNameRefListNode;
      n->ref = 1;
      n->hd = a->hd;
      r->tl = n;
      r = n;
      a = a->tl;
    }
    else
    {
      GctNameRefListNode* n = new GctNameRefListNode;
      n->ref = 1;
      n->hd = b->hd;
      r->tl = n;
      r = n;
      b = b->tl;
    }
  }
}

void GctNameRefList::sort(GctNameRefComparator f)
{
  // strategy: place runs in queue, merge runs until done
  // This is often very fast

  if (P == &NilGctNameRefListNode || P->tl == &NilGctNameRefListNode)
    return;

  int qlen = 250;   // guess a good queue size, realloc if necessary

  GctNameRefListNode** queue = (GctNameRefListNode**)malloc(qlen * sizeof(GctNameRefListNode*));

  GctNameRefListNode* h = P;
  GctNameRefListNode* a = h;
  GctNameRefListNode* b = a->tl;
  int qin = 0;

  while (b != &NilGctNameRefListNode)
  {
    if ((*f)(a->hd, b->hd) > 0)
    {
      if (h == a)               // minor optimization: ensure runlen >= 2
      {
        h = b;
        a->tl = b->tl;
        b->tl = a;
        b = a->tl;
      }
      else
      {
        if (qin >= qlen)
        {
          qlen *= 2;
          queue = (GctNameRefListNode**)realloc(queue, qlen * sizeof(GctNameRefListNode*));
        }
        queue[qin++] = h;
        a->tl = &NilGctNameRefListNode;
        h = a = b;
        b = b->tl;
      }
    }
    else
    {
      a = b;
      b = b->tl;
    }
  }

  int count = qin;
  queue[qin] = h;
  if (++qin >= qlen) qin = 0;
  int qout = 0;

  while (count-- > 0)
  {
    a = queue[qout];
    if (++qout >= qlen) qout = 0;
    b = queue[qout];
    if (++qout >= qlen) qout = 0;

    if ((*f)(a->hd, b->hd) <= 0)
    {
      h = a;
      a = a->tl;
    }
    else
    {
      h = b;
      b = b->tl;
    }
    queue[qin] = h;
    if (++qin >= qlen) qin = 0;

    for (;;)
    {
      if (a == &NilGctNameRefListNode)
      {
        h->tl = b;
        break;
      }
      else if (b == &NilGctNameRefListNode)
      {
        h->tl = a;
        break;
      }
      else if ((*f)(a->hd, b->hd) <= 0)
      {
        h->tl = a;
        h = a;
        a = a->tl;
      }
      else
      {
        h->tl = b;
        h = b;
        b = b->tl;
      }
    }
  }
  P = queue[qout];
  free(queue);
}

int GctNameRefList::list_length()
{
  GctNameRefListNode* fast = P;
  if (fast == &NilGctNameRefListNode)
    return 0;

  GctNameRefListNode* slow = fast->tl;
  if (slow == &NilGctNameRefListNode)
    return 1;

  fast = slow->tl;
  int n = 2;

  for (;;)
  {
    if (fast == &NilGctNameRefListNode)
      return n;
    else if (fast->tl == &NilGctNameRefListNode)
      return n+1;
    else if (fast == slow)
      return -1;
    else
    {
      n += 2;
      fast = fast->tl->tl;
      slow = slow->tl;
    }
  }
}

void GctNameRefList::error(const char* msg) const
{
  (*lib_error_handler)("List", msg);
}

int GctNameRefList::OK() const
{
  int v = P != 0;               // have a node
  // check that all nodes OK, even if circular:

  GctNameRefListNode* fast = P;
  if (fast != &NilGctNameRefListNode)
  {
    v &= fast->ref != 0;
    GctNameRefListNode* slow = fast->tl;
    v &= slow->ref != 0;
    if (v && slow != &NilGctNameRefListNode)
    {
      fast = slow->tl;
      v &= fast->ref != 0;
      while (v)
      {
        if (fast == &NilGctNameRefListNode)
          break;
        else if (fast->tl == &NilGctNameRefListNode)
          break;
        else if (fast == slow)
          break;
        else
        {
          v &= fast->ref != 0 && slow->ref != 0;
          fast = fast->tl->tl;
          slow = slow->tl;
        }
      }
    }
  }
  if (!v) error ("invariant failure");
  return v;
}
