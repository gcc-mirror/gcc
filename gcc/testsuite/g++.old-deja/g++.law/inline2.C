// excess errors test - XFAIL - *-*-*
// Build don't link: 
// Special g++ Options: -O2
// GROUPS passed inlining
# 1 "NISTImages.cc"
# 1 "Vision.h" 1
 




 
 
 

extern "C" {
# 1 "/sym/gnu/lib/g++-include/stdio.h" 1 3
 
 


















#pragma interface







 
 
 

# 1 "/sym/gnu/lib/g++-include/_G_config.h" 1 3
  





















 












 





 
 

# 33 "/sym/gnu/lib/g++-include/stdio.h" 2 3


extern "C" {

































# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stdio.h" 1 3

# 1 "/sym/gnu/lib/g++-include/stdarg.h" 1 3
extern "C" {
# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stdarg.h" 1 3
 






























# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/va-sparc.h" 1 3
 


 





 

typedef char * __gnuc_va_list;







 

# 79 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/va-sparc.h" 3


# 32 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stdarg.h" 2 3

# 77 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stdarg.h" 3






# 140 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stdarg.h" 3





# 2 "/sym/gnu/lib/g++-include/stdarg.h" 2 3

}
# 2 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stdio.h" 2 3

 




extern	struct	_iobuf {
	int	_cnt;
	unsigned char *_ptr;
	unsigned char *_base;
	int	_bufsiz;
	short	_flag;
	char	_file;		 
} _iob[];








































extern struct _iobuf 	*__hide_fopen ();
extern struct _iobuf 	*__hide_fdopen ();
extern struct _iobuf 	*__hide_freopen ();
extern struct _iobuf 	*__hide_popen ();
extern struct _iobuf 	*tmpfile();
extern long	__hide_ftell ();
extern char	*fgets();
extern char	*gets();
extern char	*__hide_sprintf ();
extern char	*ctermid();
extern char	*cuserid();
extern char	*__hide_tempnam ();
extern char	*tmpnam();






# 69 "/sym/gnu/lib/g++-include/stdio.h" 2 3







































}

extern "C" {

int    fclose(struct _iobuf *);
struct _iobuf *  fdopen(int, const char*);
int    fflush(struct _iobuf *);
int    fgetc(struct _iobuf *);
char*  fgets(char*, int, struct _iobuf  *);
struct _iobuf *  fopen(const char*, const char*);
int    fprintf(struct _iobuf *, const char* ...);
int    fputc(int, struct _iobuf *);
int    fputs(const char*, struct _iobuf *);
int   fread(void*, int  , int  , struct _iobuf *);



struct _iobuf *  freopen(const char*, const char*, struct _iobuf *);

int    fscanf(struct _iobuf *, const char* ...);
int    fseek(struct _iobuf *, long, int);
long   ftell(struct _iobuf  *);
int   fwrite(const void*, int  , int  , struct _iobuf *);
char*  gets(char*);
int    getw(struct _iobuf *);
int    pclose(struct _iobuf *);
void   perror(const char*);
struct _iobuf *  popen(const char*, const char*);
int    printf(const char* ...);
int    puts(const char*);
int    putw(int, struct _iobuf *);
int    rewind(struct _iobuf *);
int    scanf(const char* ...);
void   setbuf(struct _iobuf *, char*);
void   setbuffer(struct _iobuf *, char*, int);
int    setlinebuf(struct _iobuf *);
int    setvbuf(struct _iobuf *, char*, int, int  );
int    sscanf(char*, const char* ...);
struct _iobuf *  tmpfile();
int    ungetc(int, struct _iobuf *);
int    vfprintf (...) ;
int    vprintf (...) ;
char*  sprintf (...) ;
char*  vsprintf (...) ;

extern int _filbuf (...) ;
extern int _flsbuf (...) ;

}


















# 11 "Vision.h" 2

# 1 "/usr/include/floatingpoint.h" 1 3
 

 



 










# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/ieeefp.h" 1 3
 

 



 







 


enum fp_direction_type 		 
	{
	fp_nearest	= 0,
	fp_tozero	= 1,
	fp_positive	= 2,
	fp_negative	= 3
	} ;

# 34 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/ieeefp.h" 3

# 43 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/ieeefp.h" 3


# 53 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/ieeefp.h" 3

enum fp_precision_type		 
	{
	fp_extended	= 0,
	fp_single	= 1,
	fp_double	= 2,
	fp_precision_3	= 3
	} ;


# 73 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/ieeefp.h" 3

enum fp_exception_type		 
	{
	fp_inexact	= 0,
	fp_division	= 1,
	fp_underflow	= 2,
	fp_overflow	= 3,
	fp_invalid	= 4
	} ;


enum fp_class_type		 
	{
	fp_zero		= 0,
	fp_subnormal	= 1,
	fp_normal	= 2,
	fp_infinity   	= 3,
	fp_quiet	= 4,
	fp_signaling	= 5
	} ;


# 18 "/usr/include/floatingpoint.h" 2 3


 

typedef float   single;
typedef unsigned long extended[3];	 

typedef long double quadruple;	 








typedef unsigned fp_exception_field_type;
 



typedef int     sigfpe_code_type;	 

typedef void    (*sigfpe_handler_type) ();
 





 

extern enum fp_direction_type fp_direction;
 



extern enum fp_precision_type fp_precision;
 



extern fp_exception_field_type fp_accrued_exceptions;
 





 


 

typedef char    decimal_string[512 ];
 

typedef struct {
	enum fp_class_type fpclass;
	int             sign;
	int             exponent;
	decimal_string  ds;	 


	int             more;	 


	int             ndigits; 


}
                decimal_record;

enum decimal_form {
	fixed_form,		 



	floating_form		 

};

typedef struct {
	enum fp_direction_type rd;
	 
	enum decimal_form df;	 
	int             ndigits; 
}
                decimal_mode;

enum decimal_string_form {	 
	invalid_form,		 
	whitespace_form,	 
	fixed_int_form,		 
	fixed_intdot_form,	 
	fixed_dotfrac_form,	 
	fixed_intdotfrac_form,	 
	floating_int_form,	 
	floating_intdot_form,	 
	floating_dotfrac_form,	 
	floating_intdotfrac_form,	 
	inf_form,		 
	infinity_form,		 
	nan_form,		 
	nanstring_form		 
};

 

extern void     double_to_decimal();
extern void     quadruple_to_decimal();
extern char    *econvert();
extern char    *fconvert();
extern char    *gconvert();
extern char    *qeconvert();
extern char    *qfconvert();
extern char    *qgconvert();

 



extern sigfpe_handler_type ieee_handlers[5	];
 






extern sigfpe_handler_type sigfpe();

extern void     single_to_decimal();
extern void     extended_to_decimal();

extern void     decimal_to_single();
extern void     decimal_to_double();
extern void     decimal_to_extended();
extern void     decimal_to_quadruple();

extern char    *seconvert();
extern char    *sfconvert();
extern char    *sgconvert();

extern void     string_to_decimal();
extern void     file_to_decimal();
extern void     func_to_decimal();

 

extern double   atof();

 

extern int      errno;

extern double   strtod();


# 12 "Vision.h" 2

# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/malloc.h" 1 3
 




 







 


struct	mallinfo  {
	int arena;	 
	int ordblks;	 
	int smblks;	 
	int hblks;	 
	int hblkhd;	 
	int usmblks;	 
	int fsmblks;	 
	int uordblks;	 
	int fordblks;	 
	int keepcost;	 

	int mxfast;	 
	int nlblks;	 
	int grain;	 
	int uordbytes;	 
	int allocated;	 
	int treeoverhead;	 
};

typedef void *	malloc_t;

extern	malloc_t	calloc( );
extern	void	free( );
extern	malloc_t	malloc( );
extern	malloc_t	realloc( );
extern	int		mallopt();
extern	struct mallinfo mallinfo();


# 13 "Vision.h" 2

long time(long *);
int ieee_handler();
}

# 1 "/sym/gnu/lib/g++-include/std.h" 1 3
 
 





















# 1 "/sym/gnu/lib/g++-include/stddef.h" 1 3


extern "C" {



# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stddef.h" 1 3




 






 




 


 





 

# 41 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/stddef.h" 3


 





 

 



















typedef int ptrdiff_t;








 




 

 





















typedef int int  ;












 




 




















 

typedef __wchar_t wchar_t;














 











 









 








# 7 "/sym/gnu/lib/g++-include/stddef.h" 2 3




}

# 24 "/sym/gnu/lib/g++-include/std.h" 2 3

# 1 "/sym/gnu/lib/g++-include/stdlib.h" 1 3







extern "C" {

int       abs(int);


void volatile abort(void);




double    atof(const char*);
int       atoi(const char*);
long      atol(const char*);

int       atexit(auto void (*p) (void));
int       bsearch (const void *, const void *, int  , 
                   int  , auto int (*ptf)(const void*, const void*));
void*     calloc(int  , int  );
void      cfree(void*);


void volatile exit(int);




char*     fcvt(double, int, int*, int*);
void      free(void*);
char*     getenv(const char*);
int       getopt(int, char * const *, const char*);
int       getpw(int, char*);
char*     gcvt(double, int, char*);
char*     ecvt(double, int, int*, int*);
extern char**   environ;

long      labs(long);
void*     malloc(int  );
int      malloc_usable_size(void*);
int       putenv(const char*);
extern char*    optarg;
extern int      opterr;
extern int      optind;
void      qsort(void*, int  , int  , auto int (*ptf)(void*,void*));
int       rand(void);
void*     realloc(void*, int  );
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

# 25 "/sym/gnu/lib/g++-include/std.h" 2 3

# 1 "/sym/gnu/lib/g++-include/string.h" 1 3














extern "C" {

char*     strcat(char*, const char*);
char*     strchr(const char*, int);
int       strcmp(const char*, const char*);
int       strcoll(const char*, const char*);
char*     strcpy(char*, const char*);
int      strcspn(const char*, const char*);
char*     strdup(const char*);
 
 
 
 
int      strlen(const char*);
char*     strncat(char*, const char*, int  );
int       strncmp(const char*, const char*, int  );
char*     strncpy(char*, const char*, int  );
char*     strpbrk(const char*, const char*);
char*     strrchr(const char*, int);
int      strspn(const char*, const char*);
char*     strstr(const char*, const char *);
char*     strtok(char*, const char*);
int      strxfrm(char*, const char*, int  );

char*     index(const char*, int);
char*     rindex(const char*, int);
}

# 1 "/sym/gnu/lib/g++-include/memory.h" 1 3







extern "C" {

void*     memalign (...) ;
void*     memccpy (...) ;
void*     memchr (...) ;
int       memcmp (...) ;
void*     memcpy (...) ;
void*     memmove (...) ;
void*     memset (...) ;
int       ffs (...) ;
int     getpagesize (...) ;
void*     valloc (...) ;

void      bcopy (...) ;
int       bcmp (...) ;
void      bzero (...) ;
}














# 43 "/sym/gnu/lib/g++-include/string.h" 2 3



# 26 "/sym/gnu/lib/g++-include/std.h" 2 3


# 1 "/sym/gnu/lib/g++-include/unistd.h" 1 3





extern "C" {


















# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/unistd.h" 1 3
 




 


































 







 





 

















# 1 "/sym/gnu/lib/g++-include/sys/types.h" 1 3


extern "C"
{










# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/types.h" 1 3
 

 








 



# 1 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/stdtypes.h" 1 3
 

 










typedef	int		sigset_t;	 

typedef	unsigned int	speed_t;	 
typedef	unsigned long	tcflag_t;	 
typedef	unsigned char	cc_t;		 
typedef	int		pid_t;		 

typedef	unsigned short	mode_t;		 
typedef	short		nlink_t;	 

typedef	long		clock_t;	 
typedef	long		time_t;		 
















# 16 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/types.h" 2 3



# 1 "/usr/include/sys/sysmacros.h" 1 3
 

 






 


 


 



# 19 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/types.h" 2 3





typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		 
typedef	unsigned int	uint;		 















typedef	struct  _physadr_t { int r[1]; } *physadr_t;
typedef	struct label_t {
	int	val[2];
} label_t;







typedef	struct	_quad_t { long val[2]; } quad_t;
typedef	long	daddr_t;
typedef	char *	caddr_t;
typedef	unsigned long	ino_t;
typedef	short	dev_t;
typedef	long	off_t;
typedef	unsigned short	uid_t;
typedef	unsigned short	gid_t;
typedef	long	key_t;
typedef	char *	addr_t;




 









typedef	long	fd_mask;









typedef	struct fd_set {
	fd_mask	fds_bits[(((256 )+(( (sizeof (fd_mask) * 8		)	)-1))/( (sizeof (fd_mask) * 8		)	)) ];
} fd_set;







# 113 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/sys/types.h" 3



# 15 "/sym/gnu/lib/g++-include/sys/types.h" 2 3






}




# 73 "/sym/gnu/lib/gcc-lib/sparc-sun-sunos4.1/2.3.1/include/unistd.h" 2 3


extern void	_exit( );
extern int	access( );
extern unsigned	alarm( );
extern int	chdir( );
extern int	__hide_chmod ( );
extern int	__hide_chown ( );
extern int	close( );
extern char	*ctermid( );
extern char	*cuserid( );
extern int	dup( );
extern int	dup2( );
extern int	__hide_execl ( );
extern int	__hide_execle ( );
extern int	__hide_execlp ( );
extern int	execv( );
extern int	execve( );
extern int	execvp( );
extern pid_t	fork( );
extern long	fpathconf( );
extern char	*getcwd( );
extern gid_t	getegid( );
extern uid_t	geteuid( );
extern gid_t	getgid( );
extern int	getgroups( );
extern char	*getlogin( );
extern pid_t	getpgrp( );
extern pid_t	getpid( );
extern pid_t	getppid( );
extern uid_t	getuid( );
extern int	isatty( );
extern int	link( );
extern off_t	lseek( );
extern long	pathconf( );
extern int	pause( );
extern int	pipe( );
extern int	read( );
extern int	rmdir( );
extern int	__hide_setgid ( );
extern int	setpgid( );
extern pid_t	setsid( );
extern int	__hide_setuid ( );
extern unsigned	sleep( );
extern long	sysconf( );
extern pid_t	tcgetpgrp( );
extern int	tcsetpgrp( );
extern char	*ttyname( );
extern int	unlink( );
extern int	write( );



# 25 "/sym/gnu/lib/g++-include/unistd.h" 2 3















# 59 "/sym/gnu/lib/g++-include/unistd.h" 3



extern void volatile _exit(int);




extern unsigned alarm (...) ;
extern int      brk (...) ;
extern int      chdir (...) ;
extern int      chmod  (...) ;
extern int      chown (const char*, unsigned short , unsigned short );
extern int      close (...) ;
extern char*    crypt (...) ;
extern int      dup (...) ;
extern int      dup2 (...) ;
extern void	encrypt (...) ;
extern int      execl (const char*, const char *, ...);
extern int      execle (const char*, const char *, ...);
extern int      execlp (const char*, const char*, ...);
extern int      exect (...) ;
extern int      execv (...) ;
extern int      execve (...) ;
extern int      execvp (...) ;
extern int      fchown (int, unsigned short , unsigned short );
extern int  fork (...) ;
extern int      fsync (...) ;
extern int      ftruncate (...) ;
extern char*    getcwd (...) ;
extern int      getdomainname (...) ;
extern int      getdtablesize (...) ;
extern int      getgroups (...) ;
extern unsigned short  geteuid (...) ;
extern unsigned short  getegid (...) ;
extern unsigned short  getgid (...) ;
extern long     gethostid (...) ;
extern int      gethostname (...) ;
extern int  getpgrp (...) ;
extern int  getpid (...) ;
extern int  getppid (...) ;
extern char*    getlogin (...) ;
extern char*    getpass (...) ;
extern unsigned short  getuid (...) ;
extern int      ioctl (int, int, ... );
extern int      isatty (...) ;
extern int      link (...) ;
extern int      mkstemp (...) ;
extern char*    mktemp (...) ;
extern int      nice (...) ;
extern int      pause (...) ;
extern int      pipe (...) ;
extern int      readlink (...) ;
extern int      rename (...) ;
extern int      rmdir (...) ; 
extern void*    sbrk (...) ; 
extern int      syscall (...) ;
extern int      setgid (unsigned short );
extern int      sethostname (...) ;




extern int  setpgrp (...) ;

extern int      setregid (...) ;
extern int      setreuid (...) ;
extern int      setuid (unsigned short );
extern unsigned sleep (...) ;
extern void     swab (...) ;
extern int      symlink (...) ;
extern long     sysconf (...) ;
extern int      truncate (...) ;
extern char*    ttyname (...) ;
extern int      ttyslot (...) ;
 
extern int      unlink (...) ;
extern int  vfork (...) ;
extern int      vadvise (...) ;
extern int      vhangup (...) ;
extern long  lseek (...) ;
extern int  read (...) ;
extern int  write (...) ;
extern int      access (...) ;

extern int      flock (...) ;


}


# 28 "/sym/gnu/lib/g++-include/std.h" 2 3

# 1 "/sym/gnu/lib/g++-include/stdio.h" 1 3
 
 
















# 174 "/sym/gnu/lib/g++-include/stdio.h" 3

# 29 "/sym/gnu/lib/g++-include/std.h" 2 3

# 1 "/sym/gnu/lib/g++-include/errno.h" 1 3


extern "C" {





# 1 "/usr/include/errno.h" 1 3
 

 






# 1 "/usr/include/sys/errno.h" 1 3
 

 




 







































 



 



 

	 













	 













	 



 




 




 



 






 


 



 











 



# 10 "/usr/include/errno.h" 2 3

extern int errno;


# 9 "/sym/gnu/lib/g++-include/errno.h" 2 3




extern char*    sys_errlist[];
extern int      sys_nerr;

extern int      errno;

void      perror(const char*);
char*     strerr(int);


}


# 30 "/sym/gnu/lib/g++-include/std.h" 2 3

# 1 "/sym/gnu/lib/g++-include/fcntl.h" 1 3


extern "C" {










# 1 "/usr/include/fcntl.h" 1 3
 




# 1 "/usr/include/sys/fcntlcom.h" 1 3
 

 








 























 











 
 
 







 









 











 










 


 



















 









 
struct flock {
	short	l_type;		 
	short	l_whence;	 
	long	l_start;	 
	long	l_len;		 
	short	l_pid;		 
	short	l_xxx;		 
};


 
struct eflock {
	short	l_type;		 
	short	l_whence;	 
	long	l_start;	 
	long	l_len;		 
	short	l_pid;		 
	short	l_xxx;		 
	long	l_rpid;		 
	long	l_rsys;		 
};



# 1 "/sym/gnu/lib/g++-include/sys/stat.h" 1 3


extern "C"
{









# 1 "/usr/include/sys/stat.h" 1 3
 

 








struct	stat {
	dev_t	st_dev;
	ino_t	st_ino;
	mode_t	st_mode;
	short	st_nlink;
	uid_t	st_uid;
	gid_t	st_gid;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	st_atime;
	int	st_spare1;
	time_t	st_mtime;
	int	st_spare2;
	time_t	st_ctime;
	int	st_spare3;
	long	st_blksize;
	long	st_blocks;
	long	st_spare4[2];
};






















































int	__hide_chmod ( );
int	fstat( );
int	mkdir( );
int	mkfifo( );
int	stat( );
mode_t	umask( );



# 14 "/sym/gnu/lib/g++-include/sys/stat.h" 2 3






extern int       chmod  (...) ;
extern int       stat (...) ;
extern int       lstat (...) ;
extern int       fstat (...) ;


















}


# 149 "/usr/include/sys/fcntlcom.h" 2 3


int	 __hide_open ( );
int	__hide_creat ( );
int	__hide_fcntl ( );


# 6 "/usr/include/fcntl.h" 2 3





# 14 "/sym/gnu/lib/g++-include/fcntl.h" 2 3








int       fcntl(int, int, ...);
int	  creat (...) ;

int       open (...) ;


}

# 31 "/sym/gnu/lib/g++-include/std.h" 2 3


extern "C" {
int strcasecmp (...) ;
}


# 18 "Vision.h" 2

# 1 "/sym/gnu/lib/g++-include/math.h" 1 3
 
 



















#pragma interface











# 64 "/sym/gnu/lib/g++-include/math.h" 3

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
double	fmod(double, double);
double  frexp(double, int*);
double  gamma(double);
double  hypot(double,double);
double  infnan(int);

 
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



 

 


struct libm_exception
{
  int type;
  char* name;
  double arg1, arg2, retval;
};








extern "C" int matherr(libm_exception*);



# 1 "/sym/gnu/lib/g++-include/values.h" 1 3
 
 











































# 57 "/sym/gnu/lib/g++-include/values.h" 3















# 166 "/sym/gnu/lib/g++-include/values.h" 3









# 150 "/sym/gnu/lib/g++-include/math.h" 2 3


 






 






 

















































# 19 "Vision.h" 2


# 1 "MiscUtilities.h" 1
 




# 1 "/sym/gnu/lib/g++-include/std.h" 1 3
 
 

















# 37 "/sym/gnu/lib/g++-include/std.h" 3

# 6 "MiscUtilities.h" 2









inline int used_mem() {
	struct mallinfo mi = mallinfo();
	return mi.usmblks+mi.uordblks;
}
inline volatile void abort() {
	fprintf((&_iob[2]) ,"abort\n");
	exit(1);
}

inline volatile void error(char *s) {
	fprintf((&_iob[2]) ,"FATAL ERROR: ");
	fprintf((&_iob[2]) ,"%s\n",s);
	exit(1);
}
template <class Y>
struct NameValuePair {
	char *name;
	Y y;
};

template <class Y>
Y lookup(NameValuePair<Y> *data,char *name) {
	while(data->name) {
		if(!strcmp(data->name,name)) return data->y;
		data++;
	}
	abort();
	return data->y;		 
}

template <class T>
void swap(T &x,T &y) {
	T temp = x;
	x = y;
	y = temp;
}


# 21 "Vision.h" 2

# 1 "Counted.h" 1
 




# 1 "/sym/gnu/lib/g++-include/std.h" 1 3
 
 

















# 37 "/sym/gnu/lib/g++-include/std.h" 3

# 6 "Counted.h" 2


template <class T>
struct Counted {
 private:
	int *count;
	T *object;
	T *operator&() {
		return object;
	}
	void dec_count() {
		if(!count) return;
		(*count)--;
		if(*count<=0) {
			delete count;
			delete object;
			count=0;
			object=0;
		}
	}
	void inc_count() {
		if(!count) return;
		(*count)++;
	}
 public:
	Counted() {
		count=0;
		object=0;
	}
	Counted(T *object):object(object) {
		count = new int(1);
	}
	~Counted() {
		dec_count();
	}
	Counted(Counted &other) {
		count=other.count;
		object=other.object;
		inc_count();
	}
	Counted &operator=(Counted &other) {
		other.inc_count();
		dec_count();
		count=other.count;
		object=other.object;
		return *this;
	}

	T &operator*() {if(!object) abort(); else return *object; return *object;}
	T *operator->() {if(!object) abort(); else return object; return object;}
	operator T&() {if(!object) abort(); else return *object; return *object;}

	 
};


# 22 "Vision.h" 2

# 1 "Art.h" 1
 





extern "C" { void abort(); }












template <class X>
inline void art_initialize(X *,int) {}

# 39 "Art.h"


 
 
 

template <class T>
class Art {
protected:
	int mark_temp;
	T *data;
	int dims[1];
	void constructor(int d) {
		if(d<0) abort() ;
		mark_temp=0;
		dims[0]=d;
		if(d>0) {
			data=new T[d];
			if(!data) abort() ;
		} else data=0;
		art_initialize(data,d);
	}
	void destructor() {
		if(!data) return;
		delete [] data;
		mark_temp=0;
		data=0;
		dims[0]=0;
	 }
	void copy(Art &other) {
		int d0=dims[0]<?other.dims[0];
		for(int i=0;i<d0;i++) data[i]=other.data[i];
	}
public:
	void copyclear(Art &other) {
		dims[0]=other.dims[0];
		data=other.data;
		other.dims[0]=0;
		other.data=0;
	}
private:
	void copyconstructor(Art &other) {
		mark_temp=0;
		if(other.mark_temp) {
			copyclear(other);
		} else {
			constructor(other.dims[0]);
			copy(other);
		}
	}
 public:
	Art() {constructor(0);}
	Art(int d) {constructor(d);}
	Art(Art &other) {copyconstructor(other);}
	~Art() {destructor();}
	 
	Art &operator=(Art &other) {
		destructor();
		copyconstructor(other);
		return *this;
	}
	int dim(int i) {return dims[i];}
	T &operator()(int i) {
		if(unsigned(i)>=unsigned(dims[0])) abort()  ;
		return data[i];
	}
	T &sub(int i) {return data[i];}
	void resize(int nd) {
		if(nd<0) abort() ;
		Art t(nd);
		int limit=nd<?dims[0];
		for(int i=0;i<limit;i++) t.data[i]=data[i];
		t.mark_temp=1;
		*this=t;
	}
	Art &temp() { mark_temp=1; return *this; }
	T *pointer() {return data;}
	void fill(T value) {for(int i=0;i<dims[0];i++) data[i]=value;}

	int length() {return dims[0];}
	T &operator[](int i) {return operator()(i);}
};

 
 
 

 
 

template <class T>
class Stk {
protected:
	Art<T> stack;
	int fill;
public:
	Stk() {stack.resize(4); fill=0;}
	Stk(Stk &other) {
		stack=other.stack;
		fill=other.fill;
	}
	int dim(int i) {return stack.dim(i);}
	void push(T &element) {
		if(fill>=stack.dim(0)) stack.resize(2*fill);
		stack.sub(fill++)=element;
	}
	T &tos() {
		return stack(fill-1);
	}
	T &pop() {
		return stack(--fill);
	}
	T &operator()(int i) {return stack(i);}
	T &sub(int i) {return stack.sub(i);}
	void clear() {
		stack.resize(0);
		stack.resize(4);
		fill=0;
	}
	void compact() {
		stack.resize(fill+1);
	}
	Stk temp() { stack.temp(); return *this; }
	T *pointer() {return stack.pointer();}
	operator Art<T>&() {return stack;}

	int length() {return fill;}
	T &operator[](int i) {return operator()(i);}
};

 
 
 

template <class T>
class Art2 {
protected:
	int mark_temp;
	T **data;
	int dims[2];
	int total_length;
	void constructor(int d0,int d1) {
		if(d0<0||d1<0) abort() ;
		mark_temp=0;
		dims[0]=d0;
		dims[1]=d1;
		total_length=dims[0]*dims[1];
		if(d0>0) {
			data=new T*[d0];
			if(!data) abort() ;
		} else data=0;
		if(d0>0&&d1>0) {
			T *p=new T[d0*d1];
			if(!p) abort() ;
			for(int i=0;i<d0;i++) data[i]=p+i*d1;
			art_initialize(data[0],d0*d1);
		}
	}
	void destructor() {
		if(dims[0]<1) return;
		if(!data) return;
		delete [] data[0];
		delete [] data;
		mark_temp=0;
		data=0;
		dims[0]=0;
		dims[1]=0;
	}
	void copy(Art2 &other) {
		int d0=dims[0]<?other.dims[0];
		int d1=dims[1]<?other.dims[1];
		for(int i=0;i<d0;i++) for(int j=0;j<d1;j++)
			data[i][j]=other.data[i][j];
	}
public:
	void copyclear(Art2 &other) {
		dims[0]=other.dims[0];
		dims[1]=other.dims[1];
		total_length=dims[0]*dims[1];
		data=other.data;
		other.dims[0]=0;
		other.dims[1]=0;
		other.total_length=0;
		other.data=0;
	}
private:
	void copyconstructor(Art2 &other) {
		mark_temp=0;
		if(other.mark_temp) {
			copyclear(other);
		} else {
			constructor(other.dims[0],other.dims[1]);
			copy(other);
		}
	}
 public:
	Art2() {constructor(0,0);}
	Art2(int d0,int d1) {constructor(d0,d1);}
	Art2(Art2 &other) {copyconstructor(other);}
	~Art2() {destructor();}
	 
	Art2 &operator=(Art2 &other) {
		destructor();
		copyconstructor(other);
		return *this;
	}
	int dim(int i) {return dims[i];}
	T &operator()(int i,int j) {
		if(unsigned(i)>=unsigned(dims[0])||unsigned(j)>=unsigned(dims[1]))
			abort() ; 
		return data[i][j];
	}
	T &sub(int i,int j) {return data[i][j];}
	void resize(int nd0,int nd1) {
		if(nd0<0||nd1<0) abort() ;
		Art2 t(nd0,nd1);
		int limit0=nd0<?dims[0];
		int limit1=nd1<?dims[1];
		for(int i=0;i<limit0;i++) for(int j=0;j<limit1;j++) t.data[i][j]=data[i][j];
		t.mark_temp=1;
		*this=t;
	}
	Art2 &temp() { mark_temp=1; return *this; }
	T **pointer() {return data;}
	void fill(T value) {
		for(int i=0;i<dims[0];i++) for(int j=0;j<dims[1];j++)
			data[i][j]=value;
	}

	 

	int length() {return total_length;}
	T &sub(int i) {return data[0][i];}
	T &operator()(int i) {
		if(unsigned(i)>=total_length) abort() ; 
		return data[0][i];
	}
};

template <class T>
inline void art_swap(T &x,T &y) {
	T temp = x;
	x = y;
	y = temp;
}

template <class T>
inline void reverse(Art<T> &a) {
	int i;
	for(i=a.length()/2;i>=0;i--) art_swap(a.sub(i),a.sub(a.length()-i-1));
}

template <class T>
inline void reverse(Stk<T> &a) {
	reverse(a.stack);
}

template <class T>
inline void bag_remove(Stk<T> &a,T &element) {
	for(int i=0;i<a.length()-1;i++) {
		if(a(i)==element) {
			a.sub(i)=a.tos();
			a.pop();
		}
	}
	if(i<a.length()&&a.sub(i)==element) a.pop();
}

template <class T>
inline void ordered_remove(Stk<T> &a,T &element) {
	int source=0,dest=0;
	while(source<a.length()) {
		if(a(source)!=element) {
			a(dest)=a(source);
			dest++;
		}
		source++;
	}
	a.stack.resize(dest);
}

template <class T>
inline Art<T> concatenate(Art<T> &a,Art<T> &b) {
	int i,k;
	k=a.length();
	Art<T> result(k+b.length());
	for(i=0;i<a.length();i++) result.sub(i)=a.sub(i);
	for(i=0;i<b.length();i++) result.sub(i+k)=b.sub(i);
	return result.temp();
}

template <class T>
inline Stk<T> concatenate(Stk<T> &a,Stk<T> &b) {
	int i;
	Stk<T> result;
	for(i=0;i<a.length();i++) result.push(a.sub(i));
	for(i=0;i<b.length();i++) result.push(b.sub(i));
	return result.temp();
}


# 23 "Vision.h" 2

# 1 "Geo.h" 1
 




# 1 "/sym/gnu/lib/g++-include/math.h" 1 3
 
 

















# 215 "/sym/gnu/lib/g++-include/math.h" 3

# 6 "Geo.h" 2


extern "C" {
void abort();
}










 
 
 

template <class T,int n>
class vec {
protected:
	T v[n];
public:
	int length() {return n;}
	int dim(int) {return n;}
	vec() {}
	vec(T v0) { v[0]=v0; }
	vec(T v0,T v1) { v[0]=v0; v[1]=v1; }
	vec(T v0,T v1,T v2) { v[0]=v0; v[1]=v1; v[2]=v2; }
	vec(T v0,T v1,T v2,T v3) { v[0]=v0; v[1]=v1; v[2]=v2; v[3]=v3; }
	vec(T v0,T v1,T v2,T v3,T v4) { v[0]=v0; v[1]=v1; v[2]=v2; v[3]=v3; v[4]=v4; }
	
	T &operator[](int i) {return v[i];}
	T &sub(int i) {return v[i];}
	T &operator()(int i) {
		if(unsigned(i)>=n) abort(); 
		return v[i];
	}

	 
	 
	 
	 
	 
	 
	 
	 
	 

	 
	T operator*(vec &other) {
		T result=0;
		for(int i=0;i<n;i++) result=result+sub(i)*other.sub(i);
		return result;
	}

	 
	vec operator-() {
		vec result;
		for(int i=0;i<n;i++) result.sub(i)= -sub(i);
		return result;
	}
	vec operator*(T other) {
		vec result;
		for(int i=0;i<n;i++) result.sub(i)=sub(i)*other;
		return result;
	}
	vec operator/(T other) {
		vec result;
		for(int i=0;i<n;i++) result.sub(i)=sub(i)/other;
		return result;
	}
	vec operator+(vec &other) {
		vec result;
		for(int i=0;i<n;i++) result.sub(i)=sub(i)+other.sub(i);
		return result;
	}
	vec operator-(vec &other) {
		vec result;
		for(int i=0;i<n;i++) result.sub(i)=sub(i)-other.sub(i);
		return result;
	}
	vec operator<?(vec &other) {
		vec result;
		for(int i=0;i<n;i++) result.sub(i)=sub(i)<?other.sub(i);
		return result;
	}
	vec operator>?(vec &other) {
		vec result;
		for(int i=0;i<n;i++) result.sub(i)=sub(i)>?other.sub(i);
		return result;
	}

	 
	int operator==(vec &other) {
		for(int i=0;i<n;i++) if(sub(i)!=other.sub(i)) return 0;
		return 1;
	}
	int operator!=(vec &other) {
		for(int i=0;i<n;i++) if(sub(i)!=other.sub(i)) return 1;
		return 0;
	}
	int operator<(vec &other) {
		for(int i=0;i<n;i++) if(sub(i)>=other.sub(i)) return 0;
		return 1;
	}
	int operator>(vec &other) {
		for(int i=0;i<n;i++) if(sub(i)<=other.sub(i)) return 0;
		return 1;
	}
	int operator<=(vec &other) {
		for(int i=0;i<n;i++) if(sub(i)>other.sub(i)) return 0;
		return 1;
	}
	int operator>=(vec &other) {
		for(int i=0;i<n;i++) if(sub(i)<other.sub(i)) return 0;
		return 1;
	}
};

template <class T>
inline float euclidean_norm(T &v) {
	float total=0.0;
	for(int i=0;i<v.dim(0);i++) total+=v(i)*v(i);
	return sqrt(total);
}

template <class T>
inline float euclidean_distance(T &u,T &v) {
	float total=0.0;
	for(int i=0;i<u.dim(0);i++) {
		float d=u(i)-v(i);
		total+=d*d;
	}
	return sqrt(total);
}

 
 
 

template <class T,int n>
class mat:vec<T,n*n> {
protected:
public:
	int dim(int) {return n;}
	T &operator[](int i) {return v[i];}
	T &sub(int i,int j) {return v[i*n+j];}
	T &operator()(int i,int j) {
		if(unsigned(i)>=n||unsigned(j)>=n) abort(); 
		return sub(i,j);
	}
	mat operator*(mat &other) {
		mat result;
		for(int i=0;i<n;i++) for(int j=0;j<n;j++) {
			T total=0;
			for(int k=0;k<n;k++) total=total+sub(i,k)*other.sub(k,j);
			result.sub(i,j)=total;
		}
		return result;
	}
	vec<T,n> operator*(vec<T,n> &other) {
		vec<T,n> result;
		for(int i=0;i<n;i++) {
			T total=0;
			for(int k=0;k<n;k++) total=total+sub(i,k)*other.sub(k);
			result.sub(i)=total;
		}
		return result;
	}
};

 
 
 

typedef vec<float,2> vec2;
typedef vec<float,3> vec3;
typedef vec<float,4> vec4;

typedef mat<float,2> mat2;
typedef mat<float,3> mat3;
typedef mat<float,4> mat4;

typedef vec<int,2> ivec2;

 
 
 

 

inline float norm_angle(float p) {
	while(p<0) p+=2*       3.14159265358979323846 ; while(p>=2*       3.14159265358979323846 ) p-=2*       3.14159265358979323846 ; return p;
}
inline float norm_angle0(float p) {
	while(p<-       3.14159265358979323846 ) p+=2*       3.14159265358979323846 ; while(p>=       3.14159265358979323846 ) p-=2*       3.14159265358979323846 ; return p;
}
inline float norm_orientation(float p) {
	while(p<0) p+=       3.14159265358979323846 ; while(p>=       3.14159265358979323846 ) p-=       3.14159265358979323846 ; return p;
}
inline float norm_orientation0(float p) {
	while(p<-       3.14159265358979323846 /2) p+=       3.14159265358979323846 ; while(p>=       3.14159265358979323846 /2) p-=       3.14159265358979323846 ; return p;
}
inline float orientation_difference(float p,float q) {
	fabs(norm_orientation0(p-q));
}








 
 
 

 

inline vec2 cmul(vec2 &p,vec2 &q) {
	return vec2(p.sub(0)*q.sub(0)-p.sub(1)*q.sub(1),
		    p.sub(0)*q.sub(1)+p.sub(1)*q.sub(0));
}

inline vec2 cdiv(vec2 &p,vec2 &q) {
	float n=q*q;
	return vec2((p.sub(0)*q.sub(0)+p.sub(1)*q.sub(1))/n,
		    (p.sub(1)*q.sub(0)-p.sub(0)*q.sub(1))/n);
}

inline vec2 csqrt(vec2 &x) {
	if (x.sub(0)==0.0&&x.sub(1)==0.0)
		return vec2(0.0,0.0);
	else {
		float a=sqrt((fabs(x.sub(0))+hypot(x.sub(0),x.sub(1)))*0.5);
		float b=0.5*(x.sub(1)/a);
		if(x.sub(0)>0.0) return vec2(a, b);
		else if(x.sub(1)>=0.0) return vec2(a,b);
		else return vec2(-a,-b);
	}
}
inline vec2 cpow(vec2& x, double p) {
	float h=hypot(x.sub(0),x.sub(1));
	if (h<=0.0) abort();
	float lr=pow(h,p);
	float a=atan2(x.sub(1),x.sub(0));
	float li=p*a;
	return vec2(lr*cos(li),lr*sin(li));
}

 

inline float cross(vec2 p,vec2 q) {
	return p.sub(0)*q.sub(1)-p.sub(1)*q.sub(0);
}

 
 
 

struct TRS2 {
	vec2 t;
	vec2 r;
	TRS2(vec2 t=vec2(0.0,0.0),vec2 r=vec2(1.0,0.0)):t(t),r(r) {
	}
	vec2 operator()(vec2 &arg) {
		return cmul(r,arg)+t;
	}
	float rotation() {
		return atan2(r(1),r(0));
	}
};



# 24 "Vision.h" 2




# 1 "VisionTypes.h" 1
 
 
 

typedef Art2<float> FImage;
typedef Art2<int> IImage;
typedef Art2<unsigned char> CImage;

struct FImageOp {virtual FImage operator()(FImage &) = 0;};
struct FImageOp2 {virtual FImage operator()(FImage &,FImage &) = 0;};
struct CImageOp {virtual CImage operator()(CImage &) = 0;};
struct CImageOp2 {virtual CImage operator()(CImage &,CImage &) = 0;};

 
 
 

struct PFeature {
	ivec2 p;
	float a;
	short group;
	short type;
	PFeature() {}
	PFeature(ivec2 p,float a,int group,int type):p(p),a(a),group(group),type(type) {}
};

typedef Stk<PFeature> PFeatureStk;

struct FIExtractor {virtual PFeatureStk operator()(FImage &) = 0;};
struct CIExtractor {virtual PFeatureStk operator()(CImage &) = 0;};
# 28 "Vision.h" 2

# 1 "ImageDatabase.h" 1
 




# 1 "Art.h" 1
 

# 339 "Art.h"

# 6 "ImageDatabase.h" 2


typedef Art2<unsigned char> CImage;

 
 
 
 
 
 

struct ImageDatabase {
	 
	 
	 

	virtual int length() = 0;
	virtual int nclassifications() = 0;
	virtual int nusers() = 0;
	
	 
	 

	virtual int classification(int offset) = 0;
	virtual int user(int offset) = 0;
	
	virtual CImage cimage(int offset) {abort();}
	virtual PFeatureStk features(int offset) {abort();}

	 
	 
	 
	 

	virtual int user_offset(int user) = 0;
	virtual int user_length(int user) = 0;
};

ImageDatabase *make_NISTDigitImages();

 
 
 

struct ImageDatabaseIterator {
	virtual int done() = 0;
	virtual void next() = 0;
	virtual operator int() = 0;
	virtual void mark_upto_here() {}
};

ImageDatabaseIterator *make_IDI_Sequential(ImageDatabase *digits,int current,int stride);
ImageDatabaseIterator *make_IDI_OnePerUser(ImageDatabase *digits,int current);
ImageDatabaseIterator *make_IDI_FromFile(char *name);


# 29 "Vision.h" 2



# 1 "NISTImages.cc" 2










# 1 "/sym/gnu/lib/g++-include/std.h" 1 3
 
 

















# 37 "/sym/gnu/lib/g++-include/std.h" 3

# 11 "NISTImages.cc" 2

# 1 "Art.h" 1
 

# 339 "Art.h"

# 12 "NISTImages.cc" 2


extern "C" {
# 1 "./nist/ihead.h" 1


 
 
 
 
 
 
 





typedef struct ihead{
   char id[	80	];			 
   char created[	26	];		 
   char width[	8	];		 
   char height[	8	];		 
   char depth[	8	];		 
   char density[	8	];		 
   char compress[	8	];		 
   char complen[	8	];		 
   char align[	8	];		 
   char unitsize[	8	];		 
   char sigbit;				 
   char byte_order;			 
   char pix_offset[	8	];	 
   char whitepix[	8	];		 
   char issigned;			 
   char rm_cm;				 
   char tb_bt;				 
   char lr_rl;				 
   char parent[	80	];		 
   char par_x[	8	];		 
   char par_y[	8	];		 
}IHEAD;

 



















# 15 "NISTImages.cc" 2

int readihdrfile();

}

typedef Art2<unsigned char> CImage;

Stk<char*> mis_files;

extern "C" {
	char *re_comp(char *);
	int re_exec(char *);
}

struct NISTCharFile {
 private:
	NISTCharFile(NISTCharFile &);
	void operator=(NISTCharFile &);
 public:
	IHEAD *bit_header;
	int bit_w,bit_h;
	char *bit_data;
	int field_w,field_h;
	Stk<int> classes;

	static void maybe_read_mis_files() {
		if(mis_files.length()>0) return;
		struct _iobuf  *stream = fopen("/com/nist/mis-files" ,"r");
		if(!stream) abort() ;
		char buf[1024];
		while(fgets(buf,sizeof buf,stream)) {
			buf[strlen(buf)-1]='\0';
			mis_files.push(strdup(buf));
		}
		fclose(stream);
	}

	static char *find_matching_mis_file_prefix(char *pattern) {
		if(re_comp(pattern)) abort() ;
		for(int i=0;i<mis_files.length();i++) {
			if(re_exec(mis_files(i))) break;
		}
		if(i<mis_files.length()) return mis_files(i);
		else return 0;
	}

	static int part_of_user(int user) {
		int part;
		if(user<500) part=0;
		else if(user<1000) part=1;
		else if(user<1500) part=2;
		else part=3;
		return part;
	}

	static char type_of_itype(int itype) {
		char type;
		switch(itype) {
		case 0: type='d'; break;
		case 1: type='u'; break;
		case 2: type='l'; break;
		default: abort() ;
		};
		return type;
	}

	NISTCharFile(int user,char itype) {
		maybe_read_mis_files();
		int type = type_of_itype(itype);
		int part = part_of_user(user);

		char buf[512];
		sprintf(buf,"/com/nist3/data/" "hsf_%d/f%04d_.*/%c%04d_.*",part,user,type,user);
		char *prefix = find_matching_mis_file_prefix(buf);
		 ;
		
		if(!prefix) {
			bit_w = 0;
			bit_h = 0;
			field_w = 0;
			field_h = 0;
			return;
		}

		char file[512];

		strcpy(file,prefix); strcat(file,".mis");
		if(!readihdrfile(file,&bit_header,&bit_data,&bit_w,&bit_h))
			abort() ;
		field_w=atoi(bit_header->par_x);
		field_h=atoi(bit_header->par_y);

		strcpy(file,prefix); strcat(file,".cls");
		struct _iobuf  *stream = fopen(file,"r");
		if(!stream)
			abort() ;
		int total=atoi(fgets(buf,sizeof buf,stream));
		if(total!=bit_h/field_h)
			abort() ;
		while(fgets(buf,sizeof buf,stream)) classes.push(strtol(buf,0,16));
		fclose(stream);
	}

	~NISTCharFile() {
		free(bit_header);
		free(bit_data);
	}

	int length() {
		return bit_h/field_h;
	}

	CImage image(int i) {
		 ;
		CImage result(field_w,field_h);
		int bit_w8=bit_w/8;
		int offset=i*field_h;
		if(offset>=bit_h) abort() ;
		int i,j;


		for(i=0;i<field_w;i++) for(j=0;j<field_h;j++) {
			int jj=j+offset;
			result(i,field_h-j-1)=(!!(bit_data[bit_w8*jj+(i>>3)]&(1<<(7-i&7)))) ;
		}

		return result.temp();
	}

	int classification(int i) {
		return classes(i);
	}
};

int *NISTDigitImages_table;

struct NISTDigitImages:ImageDatabase {
 private:
	NISTDigitImages(NISTDigitImages&);
	void operator=(NISTDigitImages&);
 public:
	enum {table_size=2101};

	int nclassifications() {return 10;}
	int nusers() {return 2200;}

	static void init_table() {
		struct _iobuf  *stream=fopen("/com/nist/nist-digits" ,"r");
		NISTDigitImages_table = new int[table_size];
		if(!stream) abort() ;
		int i=1;
		while(fscanf(stream,"%d",&NISTDigitImages_table[i])==1) i++;
		if(i!=table_size) abort() ;
		fclose(stream);
		int total=0;
		for(i=1;i<table_size;i++) NISTDigitImages_table[i]+=NISTDigitImages_table[i-1];
	}

	static int locate(int v) {
		int low=0;
		int high=table_size;
		if(v>=NISTDigitImages_table[table_size-1]) abort() ;
		while(low+1<high) {
			int mid=(low+high)/2;
			if(NISTDigitImages_table[mid]<=v) low=mid;
			else high=mid;
		}
		while(low<table_size-1&&NISTDigitImages_table[low+1]<=v) low++;
		return low;
	}

	int user_offset(int user) {
		if(unsigned(user)>=2100) abort() ;
		return NISTDigitImages_table[user];
	}

	int user_length(int user) {
		if(unsigned(user)>=2100) abort() ;
		return NISTDigitImages_table[user+1]-NISTDigitImages_table[user];
	}

	int current_user;
	NISTCharFile *current_file;

 private:
	void get_cache(int user) {
		if(user!=current_user) {
			delete current_file;
			current_file = new NISTCharFile(user,0);
			current_user = user;
		}
	}

 public:
	NISTDigitImages() {
		 ;
		if(!NISTDigitImages_table) init_table();
		current_user=0;
		current_file=new NISTCharFile(0,0);
		 ;
	}

	~NISTDigitImages() {
		delete current_file;
	}

	int length() {return NISTDigitImages_table[table_size-1];}

	int user(int i) {
		return locate(i);
	}

	CImage image(int i) {
		int user = locate(i);
		get_cache(user);
		int offset = i-NISTDigitImages_table[user];
		return current_file->image(offset);
	}

	int classification(int i) {
		int user = locate(i);
		get_cache(user);
		int offset = i-NISTDigitImages_table[user];
		return current_file->classification(offset);
	}
};

ImageDatabase *make_NISTDigitImages() {
	return new NISTDigitImages();
}

