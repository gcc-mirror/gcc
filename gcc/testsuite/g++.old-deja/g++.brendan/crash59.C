// Build don't link: 
// Special g++ Options: -w
// GROUPS passed old-abort
typedef char * __gnuc_va_list;
void va_end (__gnuc_va_list);		 
enum __va_type_classes {
  __no_type_class = -1,
  __void_type_class,
  __integer_type_class,
  __char_type_class,
  __enumeral_type_class,
  __boolean_type_class,
  __pointer_type_class,
  __reference_type_class,
  __offset_type_class,
  __real_type_class,
  __complex_type_class,
  __function_type_class,
  __method_type_class,
  __record_type_class,
  __union_type_class,
  __array_type_class,
  __string_type_class,
  __set_type_class,
  __file_type_class,
  __lang_type_class
};
typedef __gnuc_va_list va_list;
typedef	int jmp_buf[9 ];
typedef	int sigjmp_buf[9 +1];
int	setjmp  (jmp_buf)  ;
int	_setjmp  (jmp_buf)  ;
int	sigsetjmp  (sigjmp_buf, int)  ;
void	longjmp  (jmp_buf, int)  ;
void	_longjmp  (jmp_buf, int)  ;
void	siglongjmp  (sigjmp_buf, int)  ;
const int BOR_C_DIR	= 0;		 
const int BOR_C_REV	= 1;		 
const int BOR_C		= 0x1;
const int BOR_BD_DIR	= (0<<1);	 
const int BOR_BD_REV	= (1<<1);	 
const int BOR_BD	= 0x2;
const int SERVICE_DATA	 = (1<<( 0 )) ;	 
const int SERVICE_CLASS1 = (1<<( 1 )) ;	 
const int SERVICE_CLASS2 = (1<<( 2 )) ;	 
const int SERVICE_CLASS20 = (1<<( 3 )) ;	 
const int SERVICE_VOICE	 = (1<<( 8 )) ;	 
const int SERVICE_ALL	 = (1<<( 9 )) -1;
const int VR_NORMAL	= 0;		 
const int VR_FINE	= 1;		 
const int VR_ALL	= (1<<( VR_FINE+1 )) -1;
const int BR_2400	= 0;		 
const int BR_4800	= 1;		 
const int BR_7200	= 2;		 
const int BR_9600	= 3;		 
const int BR_12000	= 4;		 
const int BR_14400	= 5;		 
const int BR_ALL	= (1<<( BR_14400+1 )) -1;
const int WD_1728	= 0;		 
const int WD_2048	= 1;		 
const int WD_2432	= 2;		 
const int WD_1216	= 3;		 
const int WD_864	= 4;		 
const int WD_ALL	= (1<<( WD_864+1 )) -1;
const int LN_A4		= 0;		 
const int LN_B4		= 1;		 
const int LN_INF	= 2;		 
const int LN_ALL	= (1<<( LN_INF+1 )) -1;
const int DF_1DMR	= 0;		 
const int DF_2DMR	= 1;		 
const int DF_2DMRUNCOMP	= 2;		 
const int DF_2DMMR	= 3;		 
const int DF_ALL	= (1<<( DF_2DMMR+1 )) -1;
const int EC_DISABLE	= 0;		 
const int EC_ENABLE	= 1;		 
const int EC_ALL	= 0x3;
const int BF_DISABLE	= 0;		 
const int BF_ENABLE	= 1;		 
const int BF_ALL	= 0x3;
const int ST_0MS	= 0;		 
const int ST_5MS	= 1;		 
const int ST_10MS2	= 2;		 
const int ST_10MS	= 3;		 
const int ST_20MS2	= 4;		 
const int ST_20MS	= 5;		 
const int ST_40MS2	= 6;		 
const int ST_40MS	= 7;		 
const int ST_ALL	= (1<<( ST_40MS+1 )) -1;
const int PPM_MPS	= 0;		 
const int PPM_EOM	= 1;		 
const int PPM_EOP	= 2;		 
const int PPM_PRI_MPS	= 4;		 
const int PPM_PRI_EOM	= 5;		 
const int PPM_PRI_EOP	= 6;		 
const int PPR_MCF	= 1;		 
const int PPR_RTN	= 2;		 
const int PPR_RTP	= 3;		 
const int PPR_PIN	= 4;		 
const int PPR_PIP	= 5;		 
const int DLE = 16;		 
const int SUB = 26;		 
const int ETX = 3;		 
const int DC1 = 17;		 
const int DC2 = 18;		 
const int CAN = 24;		 
typedef	int		sigset_t;	 
typedef	unsigned int	speed_t;	 
typedef	unsigned long	tcflag_t;	 
typedef	unsigned char	cc_t;		 
typedef	int		pid_t;		 
typedef	unsigned short	mode_t;		 
typedef	short		nlink_t;	 
typedef	long		clock_t;	 
typedef	long		time_t;		 
typedef __SIZE_TYPE__ size_t;		 
typedef int ptrdiff_t;	 
typedef __wchar_t wchar_t;	 
extern unsigned int _mb_cur_max;
extern void	abort( );
extern int	abs  (int)  ;
extern double	atof  (const char *)  ;
extern int	atoi  (const char *)  ;
extern long int	atol  (const char *)  ;
extern char *	bsearch  (const void *, const void *, long unsigned int , long unsigned int , int (*) (const void *, const void *))  ;
extern void *	calloc  (long unsigned int , long unsigned int )  ;
extern void	exit  (int)  ;
extern void	free  (void *)  ;
extern char *	getenv  (const char *)  ;
extern void *	malloc  (long unsigned int )  ;
extern int	qsort  (void *, long unsigned int , long unsigned int , int (*) (const void *, const void *))  ;
extern int	rand( );
extern void *	realloc  (void *, long unsigned int )  ;
extern int	srand  (unsigned int)  ;
extern int    mbtowc  (__wchar_t *, const char *, long unsigned int )  ;
extern int    wctomb  (char *, __wchar_t )  ;
extern size_t mbstowcs  (__wchar_t *, const char *, long unsigned int )  ;
extern size_t wcstombs  (char *, const __wchar_t *, long unsigned int )  ;
extern int atexit (void (*) (void));
extern long int labs (long int);
extern int putenv (char *);
extern double strtod (const char *, char **);
extern long int strtol (const char *, char **, int);
extern long unsigned int strtoul (const char *, char **, int);
extern int system (const char *);
extern char *	strcat  (char *, const char *)  ;
extern char *	strchr  (const char *, int)  ;
extern int	strcmp  (const char *, const char *)  ;
extern char *	strcpy  (char *, const char *)  ;
extern size_t	strcspn  (const char *, const char *)  ;
extern char *	strdup  (const char *)  ;
extern size_t	strlen  (const char *)  ;
extern char *	strncat  (char *, const char *, long unsigned int )  ;
extern int	strncmp  (const char *, const char *, long unsigned int )  ;
extern char *	strncpy  (char *, const char *, long unsigned int )  ;
extern char *	strpbrk  (const char *, const char *)  ;
extern char *	strrchr  (const char *, int)  ;
extern size_t	strspn  (const char *, const char *)  ;
extern char *	strstr  (const char *, const char *)  ;
extern char *	strtok  (char *, const char *)  ;
extern void * memchr (const void *, int, long unsigned int );
extern int memcmp (const void *, const void *, long unsigned int );
extern void * memcpy (void *, const void *, long unsigned int );
extern void * memmove (void *, const void *, long unsigned int );
extern void * memset (void *, int, long unsigned int );
extern int strcoll (const char *, const char *);
extern char * strerror (int);
extern long unsigned int strxfrm (char *, const char *, long unsigned int );
extern "C" {
extern void __eprintf (const char *, const char *, unsigned, const char *);
}
extern	struct	_iobuf {
	int	_cnt;
	unsigned char *_ptr;
	unsigned char *_base;
	int	_bufsiz;
	short	_flag;
	char	_file;		 
} _iob[];
extern struct _iobuf 	*fopen  (const char *, const char *)  ;
extern struct _iobuf 	*fdopen  (int, const char *)  ;
extern struct _iobuf 	*freopen  (const char *, const char *, struct _iobuf  *)  ;
extern struct _iobuf 	*popen  (const char *, const char *)  ;
extern struct _iobuf 	*tmpfile();
extern long	ftell  (struct _iobuf  *)  ;
extern char	*fgets  (char *, int, struct _iobuf  *)  ;
extern char	*gets  (char *)  ;
extern char	*sprintf  (char *, const char *, ...)  ;
extern char	*ctermid  (char *)  ;
extern char	*cuserid  (char *)  ;
extern char	*tempnam  (const char *, const char *)  ;
extern char	*tmpnam  (char *)  ;
extern int fclose (struct _iobuf  *);
extern int fflush (struct _iobuf  *);
extern int fgetc (struct _iobuf  *);
extern int fgetpos (struct _iobuf  *, long *);
extern int fprintf (struct _iobuf  *, const char *, ...);
extern int fputc (int, struct _iobuf  *);
extern int fputs (const char *, struct _iobuf  *);
extern long unsigned int fread (void *, long unsigned int , long unsigned int , struct _iobuf  *);
extern int fscanf (struct _iobuf  *, const char *, ...);
extern int fseek (struct _iobuf  *, long int, int);
extern int fsetpos (struct _iobuf  *, const long *);
extern long unsigned int fwrite (const void *, long unsigned int , long unsigned int , struct _iobuf  *);
extern int pclose (struct _iobuf  *);
extern void perror (const char *);
extern int printf (const char *, ...);
extern int puts (const char *);
extern int remove (const char *);
extern int rename (const char *, const char *);
extern void rewind (struct _iobuf  *);
extern int scanf (const char *, ...);
extern void setbuf (struct _iobuf  *, char *);
extern int setvbuf (struct _iobuf  *, char *, int, long unsigned int );
extern int sscanf (const char *, const char *, ...);
extern int vprintf (const char *, __gnuc_va_list);
extern int vsprintf (char *, const char *, __gnuc_va_list);
extern int vfprintf (struct _iobuf  *, const char *, __gnuc_va_list);
extern int ungetc (int, struct _iobuf  *);
extern int _flsbuf (unsigned int, struct _iobuf  *);
extern int _filbuf (struct _iobuf  *);
typedef long _G_clock_t;
typedef short _G_dev_t;
typedef long   _G_fpos_t;
typedef unsigned short _G_gid_t;
typedef unsigned long _G_ino_t;
typedef unsigned short _G_mode_t;
typedef short _G_nlink_t;
typedef long _G_off_t;
typedef int _G_pid_t;
typedef int _G_ptrdiff_t;
typedef int _G_sigset_t;
typedef long unsigned int _G_size_t;
typedef long _G_time_t;
typedef unsigned short _G_uid_t;
typedef __wchar_t _G_wchar_t;
typedef int   _G_ssize_t;
typedef int _G_wint_t;
typedef char * _G_va_list;
typedef signed char _G_int8_t;
typedef unsigned char _G_uint8_t;
typedef short _G_int16_t;
typedef unsigned short _G_uint16_t;
typedef long _G_int32_t;
typedef unsigned long _G_uint32_t;
const size_t NPOS = (size_t)(-1);
typedef void fvoid_t();
typedef _G_wint_t wint_t;
enum capacity { default_size, reserve };
extern "C" fvoid_t *set_new_handler(fvoid_t *);
extern fvoid_t *__new_handler;
extern "C" void __default_new_handler();
inline void *operator new(size_t, void *place) { return place; }
inline void *operator new[](size_t, void *place) { return place; }
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
	fd_mask	fds_bits[((( 256  )+((  (sizeof (fd_mask) * 8 )  )-1))/(  (sizeof (fd_mask) * 8 )  )) ];
} fd_set;
extern "C" {
extern int mkstemp(char *);
extern int strncasecmp(const char*, const char*, size_t);
extern int strcasecmp(const char*, const char*);
extern long random(void);
extern int seteuid(uid_t);
extern int setegid(gid_t);
extern int ftruncate(int, off_t);
extern int getdtablesize(void);
struct fd_set;
struct timeval;
extern int select(int,struct fd_set*,struct fd_set*,struct fd_set*,struct timeval*);
extern int ioctl(int, int, ...);
extern int fchown(int, uid_t, gid_t);
extern int gethostname(char*, int);
extern char* optarg;
extern int opterr;
extern int optind;
extern char* mktemp(char*);
extern void syslog(int, const char*, ...);
extern void vsyslog(int, const char*, va_list);
extern void closelog(void);
extern void openlog(const char*, int, int);
extern int fchmod(int, mode_t);
struct timezone;
struct timeval;
extern int gettimeofday(struct timeval*, struct timezone*);
extern void endpwent(void);
extern int getpeername(int,void*,int*);
extern int getsockname(int,void*,int*);
extern int socket(int, int, int);
extern int connect(int, const void*, int);
extern int flock(int, int);
}
typedef unsigned char fxBool;
inline int fxmin(int a, int b)		{ return (a < b) ? a : b; }
inline u_long fxmin(u_long a, u_long b)	{ return (a < b) ? a : b; }
inline u_int fxmin(u_int a, u_int b)	{ return (a < b) ? a : b; }
inline int fxmax(int a, int b)		{ return (a > b) ? a : b; }
inline u_long fxmax(u_long a, u_long b)	{ return (a > b) ? a : b; }
inline u_int fxmax(u_int a, u_int b)	{ return (a > b) ? a : b; }
extern "C" void _fxassert(const char*, const char*, int);
class fxObj {
public:
    fxObj();
    virtual ~fxObj();
    void inc();
    void dec();
    u_long getReferenceCount();
    virtual const char* className() const;
    int compare(const fxObj *) const;
    virtual void subClassMustDefine(const char* method) const;
protected:
    u_long	referenceCount;
};
inline fxObj::fxObj()				{ referenceCount = 0; }
inline void fxObj::inc()			{ ++referenceCount; }
inline void fxObj::dec() {
    if ( referenceCount>0 ); else _fxassert( "Bogus object reference count" ,"../util/Obj.h",54); ;
    if (0 >= --referenceCount) delete this;
}
inline u_long fxObj::getReferenceCount()	{ return referenceCount; }
class fxStr;
class fxTempStr {
public:
    fxTempStr(fxTempStr const &other);
    ~fxTempStr();
    friend fxTempStr operator|(fxStr const&, fxStr const&);
    friend fxTempStr operator|(fxStr const&, char const*);
    friend fxTempStr operator|(char const*, fxStr const&);
    friend fxTempStr& operator|(const fxTempStr&, fxStr const& b);
    friend fxTempStr& operator|(const fxTempStr&, char const* b);
    operator char*() const;
    operator int() const;
    operator float() const;
    operator double() const;
    u_int length() const;
protected:
    char	indata[100];		 
    char*	data;			 
    u_int	slength;		 
    friend class fxStr;
    fxTempStr(char const *, u_int, char const *, u_int);
    fxTempStr& concat(char const* b, u_int bl);
};
inline fxTempStr::operator char*() const	{ return data; }
inline fxTempStr::operator int() const		{ return atoi(data); }
inline fxTempStr::operator float() const	{ return float(atof(data)); }
inline fxTempStr::operator double() const	{ return double(atof(data)); }
inline u_int fxTempStr::length() const		{ return slength - 1; }
class fxStr {
    friend class fxTempStr;
public:
    fxStr(u_int l=0);
    fxStr(char const *s);
    fxStr(char const *s, u_int len);
    fxStr(fxStr const&);
    fxStr(int, char const* format);
    fxStr(long, char const* format);
    fxStr(float, char const* format);
    fxStr(double, char const* format);
    fxStr(const fxTempStr&);
    ~fxStr();
    u_long hash() const;
    operator char*() const
	{ return data; }
    operator int() const
	{ return atoi(data); }
    operator float() const
	{ return float(atof(data)); }
    operator double() const
	{ return double(atof(data)); }
    u_int length() const { return slength-1; }
    char& operator[](u_int i) const
    {   if ( i<slength-1 ); else _fxassert( "Invalid Str[] index" ,"../util/Str.h",109); ;
	return data[i]; }
    void operator=(const fxTempStr& s);
    void operator=(fxStr const& s);
    void operator=(char const *s);
    friend fxBool operator==(fxStr const&, fxStr const&);
    friend fxBool operator==(fxStr const&, char const*);
    friend fxBool operator==(fxStr const&, char *);
    friend fxBool operator==(char const*, fxStr const&);
    friend fxBool operator!=(fxStr const&, fxStr const&);
    friend fxBool operator!=(fxStr const&, char const*);
    friend fxBool operator!=(char const*, fxStr const&);
    friend fxBool operator>=(fxStr const&, fxStr const&);
    friend fxBool operator>=(fxStr const&, char const*);
    friend fxBool operator>=(char const*, fxStr const&);
    friend fxBool operator<=(fxStr const&, fxStr const&);
    friend fxBool operator<=(fxStr const&, char const*);
    friend fxBool operator<=(char const*, fxStr const&);
    friend fxBool operator>(fxStr const&, fxStr const&);
    friend fxBool operator>(fxStr const&, char const*);
    friend fxBool operator>(char const*, fxStr const&);
    friend fxBool operator<(fxStr const&, fxStr const&);
    friend fxBool operator<(fxStr const&, char const*);
    friend fxBool operator<(char const*, fxStr const&);
    int compare(fxStr const *a) const { return ::compare(*this, *a); }
    friend int compare(fxStr const&, fxStr const&);
    friend int compare(fxStr const&, char const*);
    friend int compare(char const*, fxStr const&);
    friend fxTempStr& operator|(const fxTempStr&, fxStr const&);
    friend fxTempStr& operator|(const fxTempStr&, char const*);
    friend fxTempStr operator|(fxStr const&, fxStr const&);
    friend fxTempStr operator|(fxStr const&, char const*);
    friend fxTempStr operator|(char const*, fxStr const&);
    fxStr copy() const;
    fxStr extract(u_int start,u_int len) const;
    fxStr cut(u_int start,u_int len);
    fxStr head(u_int) const;
    fxStr tail(u_int) const;
    void lowercase(u_int posn=0, u_int len=0);
    void raisecase(u_int posn=0, u_int len=0);
    void remove(u_int posn,u_int len=1);
    void resize(u_int len, fxBool reallocate = ((fxBool)0) );
    void setMaxLength(u_int maxlen);
    void append(char a);
    void append(char const *s, u_int len=0);
    void append(const fxTempStr& s)
	{ append((char*)s, s.slength-1); }
    void append(fxStr const& s)
	{ append((char*)s, s.slength-1); }
    void insert(char a, u_int posn=0);
    void insert(char const *, u_int posn=0, u_int len=0);
    void insert(const fxTempStr& s, u_int posn=0)
	{ insert((char*)s, posn, s.slength-1); }
    void insert(fxStr const& s, u_int posn=0)
	{ insert((char*)s, posn, s.slength-1); }
    u_int next(u_int posn, char delimiter) const;
    u_int next(u_int posn, char const *delimiters, u_int len=0) const;
    u_int next(u_int posn, fxStr const& delimiters) const
	{ return next(posn, (char*)delimiters, delimiters.slength-1); }
    u_int nextR(u_int posn, char delimiter) const;
    u_int nextR(u_int posn, char const*, u_int len=0) const;
    u_int nextR(u_int posn, fxStr const& delimiters) const
	{ return nextR(posn, (char*)delimiters, delimiters.slength-1); }
    u_int skip(u_int posn, char a) const; 
    u_int skip(u_int posn, char const *, u_int len=0) const;
    u_int skip(u_int posn, fxStr const& delimiters) const
	{ return skip(posn, (char*)delimiters, delimiters.slength-1); }
    u_int skipR(u_int posn, char a) const;
    u_int skipR(u_int posn, char const *, u_int len=0) const;
    u_int skipR(u_int posn, fxStr const& delimiters) const
	{ return skipR(posn, (char*)delimiters, delimiters.slength-1); }
    fxStr token(u_int & posn, char delimiter) const;
    fxStr token(u_int & posn, char const * delimiters,
	u_int delimiters_len = 0) const;
    fxStr token(u_int & posn, fxStr const & delimiters) const
	{ return token(posn, delimiters.data, delimiters.slength-1); }
    fxStr tokenR(u_int & posn, char delimiter) const;
    fxStr tokenR(u_int & posn, char const * delimiters,
	u_int delimiters_len = 0) const;
    fxStr tokenR(u_int & posn, fxStr const & delimiters) const
	{ return tokenR(posn, delimiters.data, delimiters.slength-1); }
protected:
    u_int slength;
    char * data; 
    static char emptyString;
    void fxStr::resizeInternal(u_int);
    int findEndBuffer(const char *, u_int buflen) const;
    int findBuffer(const char *buf, u_int buflen) const;
    void bracketBuffer(const char *, u_int buflen, int &, int &) const;
};
struct Class2Params {
    u_int vr;		 
    u_int br;		 
    u_int wd;		 
    u_int ln;		 
    u_int df;		 
    u_int ec;		 
    u_int bf;		 
    u_int st;		 
    static u_int vrDISTab[2];		 
    static u_int dfDISTab[4];		 
    static u_int wdDISTab[8];		 
    static u_int lnDISTab[3];		 
    static u_int stDISTab[8];		 
    static u_int stDCSTab[8];		 
    static u_int brDISTab[8];		 
    static u_int brDCSTab[8];		 
    static u_int DISvrTab[2];		 
    static u_int DISdfTab[2];		 
    static u_int DISwdTab[4];		 
    static u_int DISlnTab[4];		 
    static u_int DISstTab[8];		 
    static u_int DISbrTab[16];		 
    static u_int DCSbrTab[16];		 
    static const char* bitRateNames[8];		 
    static const char* dataFormatNames[4];	 
    static const char* vresNames[2];		 
    static const char* pageWidthNames[8];	 
    static const char* pageLengthNames[4];	 
    static const char* scanlineTimeNames[8];	 
    Class2Params();
    int operator==(const Class2Params&) const;
    int operator!=(const Class2Params&) const;
    fxStr cmd() const;			 
    void setFromDIS(u_int dis, u_int xinfo = 0);
    void setFromDCS(u_int dcs, u_int xinfo = 0);
    u_int getDCS() const;
    fxBool is2D() const;
    u_int pageWidth() const;
    u_int transferSize(u_int ms) const;
    u_int minScanlineSize() const;
    fxStr encode() const;		 
    void decode(const fxStr&);		 
};
typedef	struct {
	unsigned short tiff_magic;	 
	unsigned short tiff_version;	 
	unsigned long  tiff_diroff;	 
} TIFFHeader;
typedef	struct {
	unsigned short tdir_tag;	 
	unsigned short tdir_type;	 
	unsigned long  tdir_count;	 
	unsigned long  tdir_offset;	 
} TIFFDirEntry;
typedef	enum {
	TIFF_NOTYPE	= 0,	 
	TIFF_BYTE	= 1,	 
	TIFF_ASCII	= 2,	 
	TIFF_SHORT	= 3,	 
	TIFF_LONG	= 4,	 
	TIFF_RATIONAL	= 5,	 
	TIFF_SBYTE	= 6,	 
	TIFF_UNDEFINED	= 7,	 
	TIFF_SSHORT	= 8,	 
	TIFF_SLONG	= 9,	 
	TIFF_SRATIONAL	= 10,	 
	TIFF_FLOAT	= 11,	 
	TIFF_DOUBLE	= 12	 
} TIFFDataType;
typedef	struct tiff TIFF;
extern int fclose (struct _iobuf  *);
extern int fflush (struct _iobuf  *);
extern int fgetc (struct _iobuf  *);
extern int fgetpos (struct _iobuf  *, long *);
extern int fprintf (struct _iobuf  *, const char *, ...);
extern int fputc (int, struct _iobuf  *);
extern int fputs (const char *, struct _iobuf  *);
extern long unsigned int fread (void *, long unsigned int , long unsigned int , struct _iobuf  *);
extern int fscanf (struct _iobuf  *, const char *, ...);
extern int fseek (struct _iobuf  *, long int, int);
extern int fsetpos (struct _iobuf  *, const long *);
extern long unsigned int fwrite (const void *, long unsigned int , long unsigned int , struct _iobuf  *);
extern int pclose (struct _iobuf  *);
extern void perror (const char *);
extern int printf (const char *, ...);
extern int puts (const char *);
extern int remove (const char *);
extern int rename (const char *, const char *);
extern void rewind (struct _iobuf  *);
extern int scanf (const char *, ...);
extern void setbuf (struct _iobuf  *, char *);
extern int setvbuf (struct _iobuf  *, char *, int, long unsigned int );
extern int sscanf (const char *, const char *, ...);
extern int vprintf (const char *, __gnuc_va_list);
extern int vsprintf (char *, const char *, __gnuc_va_list);
extern int vfprintf (struct _iobuf  *, const char *, __gnuc_va_list);
extern int ungetc (int, struct _iobuf  *);
extern int _flsbuf (unsigned int, struct _iobuf  *);
extern int _filbuf (struct _iobuf  *);
typedef	void (*TIFFErrorHandler)(const char* module, const char* fmt, va_list);
typedef	int (*TIFFReadWriteProc)(void*, char*, unsigned long);
typedef	long (*TIFFSeekProc)(void*, long, int);
typedef	int (*TIFFCloseProc)(void*);
typedef	long (*TIFFSizeProc)(void*);
typedef	int (*TIFFMapFileProc)(void*, char**, long*);
typedef	void (*TIFFUnmapFileProc)(void*, char*, long);
extern "C" {
extern	const char* TIFFGetVersion();
extern	void TIFFClose(TIFF*);
extern	int TIFFFlush(TIFF*);
extern	int TIFFFlushData(TIFF*);
extern	int TIFFGetField(TIFF*, unsigned int, ...);
extern	int TIFFVGetField(TIFF*, unsigned int, va_list);
extern	int TIFFGetFieldDefaulted(TIFF*, unsigned int, ...);
extern	int TIFFVGetFieldDefaulted(TIFF*, unsigned int, va_list);
extern	int TIFFReadDirectory(TIFF*);
extern	unsigned long TIFFScanlineSize(TIFF*);
extern	unsigned long TIFFStripSize(TIFF*);
extern	unsigned long TIFFVStripSize(TIFF*, unsigned long);
extern	unsigned long TIFFTileRowSize(TIFF*);
extern	unsigned long TIFFTileSize(TIFF*);
extern	unsigned long TIFFVTileSize(TIFF*, unsigned long);
extern	int TIFFFileno(TIFF*);
extern	int TIFFGetMode(TIFF*);
extern	int TIFFIsTiled(TIFF*);
extern	long TIFFCurrentRow(TIFF*);
extern	int TIFFCurrentDirectory(TIFF*);
extern	int TIFFCurrentStrip(TIFF*);
extern	int TIFFCurrentTile(TIFF*);
extern	int TIFFReadBufferSetup(TIFF*, char*, unsigned long);
extern	int TIFFLastDirectory(TIFF*);
extern	int TIFFSetDirectory(TIFF*, int);
extern	int TIFFSetField(TIFF*, unsigned int, ...);
extern	int TIFFVSetField(TIFF*, unsigned int, va_list);
extern	int TIFFWriteDirectory(TIFF *);
extern	void TIFFPrintDirectory(TIFF*, struct _iobuf *, long = 0);
extern	int TIFFReadScanline(TIFF*, unsigned char*, unsigned, unsigned = 0);
extern	int TIFFWriteScanline(TIFF*, unsigned char*, unsigned, unsigned = 0);
extern	int TIFFReadRGBAImage(TIFF*,
	    unsigned long, unsigned long, unsigned long*, int stop = 0);
extern	TIFF* TIFFOpen(const char*, const char*);
extern	TIFF* TIFFFdOpen(int, const char*, const char*);
extern	TIFF* TIFFClientOpen(const char* name, const char* mode,
	    void* clientdata,
	    TIFFReadWriteProc readproc, TIFFReadWriteProc writeproc,
	    TIFFSeekProc seekproc, TIFFCloseProc closeproc,
	    TIFFSizeProc sizeproc,
	    TIFFMapFileProc mapproc, TIFFUnmapFileProc unmapproc);
extern	const char* TIFFFileName(TIFF*);
extern	void TIFFError(const char*, const char*, ...);
extern	void TIFFWarning(const char*, const char*, ...);
extern	TIFFErrorHandler TIFFSetErrorHandler(TIFFErrorHandler handler);
extern	TIFFErrorHandler TIFFSetWarningHandler(TIFFErrorHandler handler);
extern	unsigned int TIFFComputeTile(TIFF*,
	    unsigned long, unsigned long, unsigned long, unsigned int);
extern	int TIFFCheckTile(TIFF*,
	    unsigned long, unsigned long, unsigned long, unsigned);
extern	unsigned int TIFFNumberOfTiles(TIFF*);
extern	int TIFFReadTile(TIFF*,
	    unsigned char*,
	    unsigned long, unsigned long, unsigned long,
	    unsigned int);
extern	int TIFFWriteTile(TIFF*,
	    unsigned char*,
	    unsigned long, unsigned long, unsigned long,
	    unsigned int);
extern	unsigned int TIFFComputeStrip(TIFF*, unsigned long, unsigned int);
extern	unsigned int TIFFNumberOfStrips(TIFF*);
extern	int TIFFReadEncodedStrip(TIFF*, unsigned, unsigned char*, unsigned long);
extern	int TIFFReadRawStrip(TIFF*, unsigned, unsigned char*, unsigned long);
extern	int TIFFReadEncodedTile(TIFF*, unsigned, unsigned char*, unsigned long);
extern	int TIFFReadRawTile(TIFF*, unsigned, unsigned char*, unsigned long);
extern	int TIFFWriteEncodedStrip(TIFF*, unsigned, unsigned char*, unsigned long);
extern	int TIFFWriteRawStrip(TIFF*, unsigned, unsigned char*, unsigned long);
extern	int TIFFWriteEncodedTile(TIFF*, unsigned, unsigned char*, unsigned long);
extern	int TIFFWriteRawTile(TIFF*, unsigned, unsigned char*, unsigned long);
extern	void TIFFSetWriteOffset(TIFF*, unsigned long);
extern	void TIFFSwabShort(unsigned short *);
extern	void TIFFSwabLong(unsigned long *);
extern	void TIFFSwabArrayOfShort(unsigned short *, unsigned long);
extern	void TIFFSwabArrayOfLong(unsigned long *, unsigned long);
extern	void TIFFReverseBits(unsigned char *, unsigned long);
extern	const unsigned char* TIFFGetBitRevTable(int);
}
struct G3Base {
    short	data;		 
    short	bit;		 
    fxBool	is2D;		 
    enum { G3_1D, G3_2D } tag;	 
    const u_char* bitmap;	 
    static const u_char zeroruns[256];
    static const u_char oneruns[256];
    static int findspan(const u_char**, int, int, const u_char*);
    static int finddiff(const u_char*, int, int);
    static void fillspan(u_char* cp, int x, int count);
    void setup(const u_char*, fxBool is2d);
};
class fxStackBuffer;
class G3Decoder : private G3Base {
private:
    short	bytePending;	 
    short	prevByte;	 
    u_char*	refline;	 
    fxStackBuffer* recvBuf;	 
    fxBool	decode1DRow(u_char*, u_int);
    fxBool	decode2DRow(u_char*, u_int);
    int		nextBit();
    void	ungetBit();
    int		nextByte();
    int		decodeWhiteRun();
    int		decodeBlackRun();
    int		decodeUncompCode();
    void	skipToEOL(int len);
protected:
    G3Decoder();
    void	raiseEOF();
    void	raiseRTC();
    void	setPendingByte(u_char);
    virtual int decodeNextByte() = 0;
    void	setRefLine(u_char*);
    u_char*	getRefLine();
    virtual void invalidCode(const char* type, int x);
    virtual void prematureEOL(const char* type, int x);
    virtual void badPixelCount(const char* type, int x);
    virtual void badDecodingState(const char* type, int x);
public:
    sigjmp_buf	jmpEOF;		 
    sigjmp_buf	jmpRTC;		 
    virtual ~G3Decoder();
    void	setupDecoder(u_int, fxBool is2D);
    void	decode(void* raster, u_int w, u_int h);
    void	skip(u_int h);
    void	skipLeader();
    fxBool	decodeRow(void* scanline, u_int w);
    void	skipRow();
    fxBool	isLastRow1D();
    fxBool	isNextRow1D();
    fxBool	isByteAligned();
    void	setRecvBuf(fxStackBuffer&);
    fxStackBuffer* getRecvBuf();
    void	flushRecvBuf();
};
inline void G3Decoder::setRecvBuf(fxStackBuffer& b){ recvBuf = &b; }
inline fxStackBuffer* G3Decoder::getRecvBuf()	{ return recvBuf; }
inline void G3Decoder::setRefLine(u_char* b)	{ refline = b; }
inline u_char* G3Decoder::getRefLine()		{ return refline; }
inline fxBool G3Decoder::isLastRow1D()		{ return tag == G3_1D; }
enum FaxSendStatus {
    send_retry,			 
    send_failed,		 
    send_done,			 
    send_reformat,		 
};
class FaxServer;
class FaxMachineInfo;
class ModemConfig;
class fxStackBuffer;
class FaxFont;
class FaxRequest;
typedef unsigned int CallStatus;	 
typedef	unsigned int CallType;		 
typedef	unsigned int AnswerType;	 
typedef unsigned int SpeakerVolume;
typedef	unsigned int ATResponse;	 
typedef	unsigned int BaudRate;		 
typedef	unsigned int FlowControl;	 
typedef	unsigned int SetAction;		 
typedef struct {
    const char*	msg;		 
    u_short	len;		 
    ATResponse	expect;		 
    CallStatus	status;		 
    CallType	type;		 
} AnswerMsg;
class FaxModem : public G3Decoder {
public:
    static FaxModem* deduceModem(FaxServer&, const ModemConfig& config);
    enum {			 
	OK	   = 0,		 
	BUSY	   = 1,		 
	NOCARRIER  = 2,		 
	NOANSWER   = 3,		 
	NODIALTONE = 4,		 
	ERROR	   = 5,		 
	FAILURE	   = 6,		 
	NOFCON	   = 7,		 
	DATACONN   = 8,		 
    };
    enum {			 
	CALLTYPE_ERROR	= 0,	 
	CALLTYPE_DATA	= 1,	 
	CALLTYPE_FAX	= 2,	 
	CALLTYPE_VOICE	= 3,	 
	CALLTYPE_UNKNOWN = 4,	 
    };
    enum {			 
	OFF	= 0,		 
	QUIET	= 1,		 
	LOW	= 2,		 
	MEDIUM	= 3,		 
	HIGH	= 4,		 
    };
    enum {			 
	BR0	= 0,		 
	BR300	= 1,		 
	BR1200	= 2,		 
	BR2400	= 3,		 
	BR4800	= 4,		 
	BR9600	= 5,		 
	BR19200	= 6,		 
	BR38400	= 7,		 
	BR57600	= 8,		 
	BR76800	= 9,		 
    };
    enum {			 
	FLOW_NONE	= 0,	 
	FLOW_XONXOFF	= 1,	 
	FLOW_RTSCTS	= 2	 
    };
    enum {			 
	ACT_NOW		= 0,	 
	ACT_DRAIN	= 1,	 
	ACT_FLUSH	= 2,	 
    };
    enum {			 
	ANSTYPE_ANY	= 0,	 
	ANSTYPE_DATA	= 1,	 
	ANSTYPE_FAX	= 2,	 
	ANSTYPE_VOICE	= 3,	 
    };
    enum {			 
	AT_NOTHING	= 0,	 
	AT_OK		= 1,	 
	AT_CONNECT	= 2,	 
	AT_NOANSWER	= 3,	 
	AT_NOCARRIER	= 4,	 
	AT_NODIALTONE	= 5,	 
	AT_BUSY		= 6,	 
	AT_OFFHOOK	= 7,	 
	AT_RING		= 8,	 
	AT_ERROR	= 9,	 
	AT_EMPTYLINE	= 10,	 
	AT_TIMEOUT	= 11,	 
	AT_OTHER	= 12,	 
    };
private:
    FaxServer&	server;		 
    fxStr	resetCmds;	 
    long	dataTimeout;	 
    BaudRate	rate;		 
    FlowControl	iFlow;		 
    FlowControl	oFlow;		 
    u_int	pageNumber;	 
    FaxFont*	tagLineFont;	 
    u_int	tagLineSlop;	 
    fxStr	tagLine;	 
    u_int	tagLineFields;	 
    void	setupTagLine(const FaxRequest&);
    u_int	cblc;		 
    fxBool	lastRowBad;	 
    u_long	recvEOLCount;	 
    u_long	recvBadLineCount;
    u_long	recvConsecutiveBadLineCount;
    u_int	recvCC;		 
    fxStackBuffer* recvBuf;	 
    u_long	savedWriteOff;	 
    void	recvRow(TIFF* tif, fxStackBuffer& row, u_char* buf);
    void	invalidCode(const char* type, int x);
    void	prematureEOL(const char* type, int x);
    void	badPixelCount(const char* type, int x);
    void	badDecodingState(const char* type, int x);
protected:
    const ModemConfig& conf;	 
    FlowControl	flowControl;	 
    u_int	modemServices;	 
    fxStr	modemMfr;	 
    fxStr	modemModel;	 
    fxStr	modemRevision;	 
    Class2Params modemParams;	 
    char	rbuf[1024];	 
    ATResponse	lastResponse;	 
    fxStr	mfrQueryCmd;	 
    fxStr	modelQueryCmd;	 
    fxStr	revQueryCmd;	 
    static const char* serviceNames[9];	  
    static const u_char digitMap[12*2+1]; 
    static const char* callStatus[9];	  
    FaxModem(FaxServer&, const ModemConfig&);
    virtual fxBool selectBaudRate(BaudRate max, FlowControl i, FlowControl o);
    virtual fxBool setupModem() = 0;
    virtual fxBool setupManufacturer(fxStr& mfr);
    virtual fxBool setupModel(fxStr& model);
    virtual fxBool setupRevision(fxStr& rev);
    fxBool doQuery(const fxStr& queryCmd, fxStr& result, long ms = 30*1000);
    virtual const AnswerMsg* findAnswer(const char* s);
    virtual CallType answerResponse(fxStr& emsg);
    virtual CallStatus dialResponse(fxStr& emsg) = 0;
    void	setDataTimeout(long secs, u_int br);
    long	getDataTimeout() const;
    void	pause(u_int ms);
    void	countPage();
    void	modemTrace(const char* fmt, ...);
    void	modemSupports(const char* fmt, ...);
    void	modemCapability(const char* fmt, ...);
    void	protoTrace(const char* fmt, ...);
    void	recvTrace(const char* fmt, ...);
    void	copyQualityTrace(const char* fmt, ...);
    void	serverTrace(const char* fmt, ...);
    void	traceBits(u_int bits, const char* bitNames[]);
    void	traceModemParams();
    void	tracePPR(const char* dir, u_int ppr);
    void	tracePPM(const char* dir, u_int ppm);
    void	trimModemLine(char buf[], int& cc);
    int		getModemLine(char buf[], u_int bufSize, long ms = 0);
    void	beginTimedTransfer();
    void	endTimedTransfer();
    fxBool	wasTimeout();
    void	setTimeout(fxBool);
    void	flushModemInput();
    fxBool	putModem(void* data, int n, long ms = 0);
    fxBool	putModemData(void* data, int n);
    fxBool	putModemDLEData(const u_char* data, u_int,
		    const u_char* brev, long ms);
    fxBool	putModemLine(const char* cp);
    int		getModemChar(long ms = 0);
    int		getModemDataChar();
    void	startTimeout(long ms);
    void	stopTimeout(const char* whichdir);
    static const char* ATresponses[13];
    virtual ATResponse atResponse(char* buf, long ms = 30*1000);
    virtual fxBool waitFor(ATResponse wanted, long ms = 30*1000);
    fxBool	atCmd(const fxStr& cmd, ATResponse = AT_OK, long ms = 30*1000);
    fxBool	atQuery(const char* what, u_int& v, long ms = 30*1000);
    fxBool	atQuery(const char* what, fxStr& v, long ms = 30*1000);
    u_int	fromHex(const char*, int = -1);
    fxStr	toHex(int, int ndigits);
    fxBool	parseRange(const char*, u_int&);
    fxBool	vparseRange(const char*, int nargs ...);
    fxBool	vatFaxCmd(ATResponse resp, const char* cmd ... );
    fxBool	sendBreak(fxBool pause);
    fxBool	setBaudRate(BaudRate rate);
    fxBool	setBaudRate(BaudRate rate, FlowControl i, FlowControl o);
    fxBool	setXONXOFF(FlowControl i, FlowControl o, SetAction);
    fxBool	setDTR(fxBool on);
    fxBool	setInputBuffering(fxBool on);
    fxBool	modemStopOutput();
    FlowControl	getInputFlow();
    FlowControl	getOutputFlow();
    fxBool	getProtocolTracing();
    fxBool	getHDLCTracing();
    FaxSendStatus sendSetupParams(TIFF*, Class2Params&,
		    FaxMachineInfo&, fxStr&);
    fxBool	recvCheckTSI(const fxStr&);
    void	recvCSI(fxStr&);
    void	recvDCS(Class2Params&);
    void	recvNSF(u_int nsf);
    void	recvSetupPage(TIFF* tif, long group3opts, int fillOrder);
    void	recvResetPage(TIFF* tif);
    fxBool	abortRequested();
    fxBool	decodePPM(const fxStr& pph, u_int& ppm, fxStr& emsg);
    fxBool	recvPageDLEData(TIFF* tif, fxBool checkQuality,
		    const Class2Params& params, fxStr& emsg);
    virtual void abortPageRecv() = 0;
    virtual int decodeNextByte();
    fxBool	checkQuality();
    fxBool	isQualityOK(const Class2Params&);
    u_long	getRecvEOLCount() const;
    u_long	getRecvBadLineCount() const;
    u_long	getRecvConsecutiveBadLineCount() const;
    fxBool	setupTagLineSlop(const Class2Params&);
    u_int	getTagLineSlop() const;
    u_char*	imageTagLine(u_char* buf, u_int fillorder, const Class2Params&);
public:
    virtual ~FaxModem();
    virtual fxBool dataService();		 
    virtual fxBool voiceService();		 
    virtual fxBool sync(long ms = 0);		 
    virtual fxBool reset(long ms = 5*1000);	 
    virtual void hangup();			 
    virtual void setSpeakerVolume(SpeakerVolume);
    virtual void setLID(const fxStr& number) = 0;
    const fxStr& getModel() const;
    const fxStr& getManufacturer() const;
    const fxStr& getRevision() const;
    virtual fxBool supports2D() const;
    virtual fxBool supportsEOLPadding() const;
    virtual fxBool supportsVRes(float res) const;
    virtual fxBool supportsPageWidth(u_int w) const;
    virtual fxBool supportsPageLength(u_int l) const;
    virtual fxBool supportsPolling() const;
    virtual int selectSignallingRate(int br) const;
    u_int getBestSignallingRate() const;
    u_int getBestScanlineTime() const;
    virtual int selectScanlineTime(int st) const;
    u_int getBestVRes() const;
    u_int getBestDataFormat() const;
    u_int getBestPageWidth() const;
    u_int getBestPageLength() const;
    u_int modemDIS() const;
    virtual CallStatus dial(const char* number,
	const Class2Params& dis, fxStr& emsg);
    virtual void sendBegin(const FaxRequest&);
    virtual fxBool getPrologue(Class2Params&,
	u_int& nsf, fxStr& csi, fxBool& hasDoc) = 0;
    virtual void sendSetupPhaseB();
    virtual FaxSendStatus sendPhaseB(TIFF*, Class2Params&, FaxMachineInfo&,
	fxStr& pph, fxStr& emsg) = 0;
    virtual void sendEnd();
    virtual void sendAbort() = 0;
    virtual fxBool waitForRings(u_int rings);
    virtual CallType answerCall(AnswerType, fxStr& emsg);
    virtual fxBool recvBegin(fxStr& emsg) = 0;
    virtual fxBool recvPage(TIFF*, int& ppm, fxStr& em) = 0;
    virtual fxBool recvEnd(fxStr& emsg) = 0;
    virtual void recvAbort() = 0;
    virtual fxBool requestToPoll() = 0;
    virtual fxBool pollBegin(const fxStr& pollID, fxStr& emsg) = 0;
};
inline long FaxModem::getDataTimeout() const		{ return dataTimeout; }
inline const fxStr& FaxModem::getModel() const		{ return modemModel; }
inline const fxStr& FaxModem::getManufacturer() const	{ return modemMfr; }
inline const fxStr& FaxModem::getRevision() const	{ return modemRevision; }
inline FlowControl FaxModem::getInputFlow()		{ return iFlow; }
inline FlowControl FaxModem::getOutputFlow()		{ return oFlow; }
inline u_int FaxModem::getTagLineSlop() const		{ return tagLineSlop; }
struct ModemConfig {
    fxStr	type;			 
    fxStr	resetCmds;		 
    fxStr	dialCmd;		 
    fxStr	answerAnyCmd;		 
    fxStr	answerDataCmd;		 
    fxStr	answerFaxCmd;		 
    fxStr	answerVoiceCmd;		 
    fxStr	flowControlCmd;		 
    fxStr	setupDTRCmd;		 
    fxStr	setupDCDCmd;		 
    fxStr	setupAACmd;		 
    fxStr	noAutoAnswerCmd;	 
    fxStr	setVolumeCmd[5];	 
    fxStr	echoOffCmd;		 
    fxStr	verboseResultsCmd;	 
    fxStr	resultCodesCmd;		 
    fxStr	onHookCmd;		 
    fxStr	softResetCmd;		 
    fxStr	waitTimeCmd;		 
    fxStr	pauseTimeCmd;		 
    fxStr	mfrQueryCmd;		 
    fxStr	modelQueryCmd;		 
    fxStr	revQueryCmd;		 
    fxStr	answerAnyBeginCmd;	 
    fxStr	answerDataBeginCmd;	 
    fxStr	answerFaxBeginCmd;	 
    fxStr	answerVoiceBeginCmd;	 
    fxStr	sendBeginCmd;		 
    u_int	t1Timer;		 
    u_int	t2Timer;		 
    u_int	t4Timer;		 
    u_int	dialResponseTimeout;	 
    u_int	answerResponseTimeout;	 
    u_int	pageStartTimeout;	 
    u_int	pageDoneTimeout;	 
    fxStr	class1Cmd;		 
    u_int	class1TCFResponseDelay;	 
    u_int	class1SendPPMDelay;	 
    u_int	class1SendTCFDelay;	 
    u_int	class1TrainingRecovery;	 
    u_int	class1RecvAbortOK;	 
    u_int	class1FrameOverhead;	 
    u_int	class1RecvIdentTimer;	 
    u_int	class1TCFMaxNonZero;	 
    u_int	class1TCFMinRun;	 
    fxStr	class2Cmd;		 
    fxStr	class2DCCQueryCmd;	 
    fxStr	class2CQQueryCmd;	 
    fxStr	class2BORCmd;		 
    fxStr	class2RELCmd;		 
    fxStr	class2CQCmd;		 
    fxStr	class2AbortCmd;		 
    fxStr	class2TBCCmd;		 
    fxStr	class2CRCmd;		 
    fxStr	class2PHCTOCmd;		 
    fxStr	class2BUGCmd;		 
    fxStr	class2LIDCmd;		 
    fxStr	class2DCCCmd;		 
    fxStr	class2DISCmd;		 
    fxStr	class2DDISCmd;		 
    fxStr	class2CIGCmd;		 
    fxStr	class2SPLCmd;		 
    fxStr	class2PTSCmd;		 
    fxStr	class2RecvDataTrigger;	 
    fxBool	class2XmitWaitForXON;	 
    fxStr	class2PIECmd;		 
    fxStr	class2NRCmd;		 
    FlowControl	flowControl;		 
    BaudRate	maxRate;		 
    u_int	recvFillOrder;		 
    u_int	sendFillOrder;		 
    u_int	frameFillOrder;		 
    u_int	resetDelay;		 
    u_int	baudRateDelay;		 
    u_int	maxPacketSize;		 
    u_int	interPacketDelay;	 
    u_int	percentGoodLines;	 
    u_int	maxConsecutiveBadLines;	 
    fxBool	waitForConnect;		 
    fxStr	tagLineFmt;		 
    fxStr	tagLineFontFile;	 
    ModemConfig();
    ~ModemConfig();
    fxBool parseItem(const char* tag, const char* value);
    void setVolumeCmds(const fxStr& value);
    fxStr parseATCmd(const char*);
};
ModemConfig::ModemConfig()
    : type("unknown")
    , dialCmd("DT%s")			 
    , noAutoAnswerCmd("S0=0")
    , echoOffCmd("E0")
    , verboseResultsCmd("V1")
    , resultCodesCmd("Q0")
    , onHookCmd("H0")
    , softResetCmd("Z")
    , waitTimeCmd("S7=30")		 
    , pauseTimeCmd("S8=2")		 
    , class1Cmd("+FCLASS=1")		 
    , class2CQQueryCmd("+FCQ=?")	 
    , tagLineFmt("From %%n|%c|Page %%p of %%t")
{
    class2XmitWaitForXON = ((fxBool)1) ;	 
    setVolumeCmds("M0 L0M1 L1M1 L2M1 L3M1");
    answerAnyCmd = "A";
    flowControl = FaxModem::FLOW_NONE;	 
    maxRate = FaxModem::BR19200;	 
    sendFillOrder = 2 ;	 
    recvFillOrder = 2 ;	 
    frameFillOrder = 2 ;	 
    resetDelay = 2600;			 
    baudRateDelay = 0;			 
    t1Timer = ((35+5)*1000) ;			 
    t2Timer = ((6+1)*1000) ;			 
    t4Timer = 3100 ;			 
    dialResponseTimeout = 3*60*1000;	 
    answerResponseTimeout = 3*60*1000;	 
    pageStartTimeout = 3*60*1000;	 
    pageDoneTimeout = 3*60*1000;	 
    class1TCFResponseDelay = 75;	 
    class1SendPPMDelay = 75;		 
    class1SendTCFDelay = 75;		 
    class1TrainingRecovery = 1500;	 
    class1RecvAbortOK = 200;		 
    class1FrameOverhead = 4;		 
    class1RecvIdentTimer = t1Timer;	 
    class1TCFMinRun = (2*1500 )/3; 
    class1TCFMaxNonZero = 10;		 
    maxPacketSize = 16*1024;		 
    interPacketDelay = 0;		 
    waitForConnect = ((fxBool)0) ;		 
    percentGoodLines = 95;		 
    maxConsecutiveBadLines = 5;		 
}
ModemConfig::~ModemConfig()
{
}
static fxBool getBoolean(const char* cp)
    { return ((strcasecmp( cp ,  "on" )==0)  || (strcasecmp( cp ,  "yes" )==0) ); }
static BaudRate
findRate(const char* cp)
{
    static const struct {
	const char* name;
	BaudRate    br;
    } rates[] = {
	{   "300", FaxModem::BR300 },
	{  "1200", FaxModem::BR1200 },
	{  "2400", FaxModem::BR2400 },
	{  "4800", FaxModem::BR4800 },
	{  "9600", FaxModem::BR9600 },
	{ "19200", FaxModem::BR19200 },
	{ "38400", FaxModem::BR38400 },
	{ "57600", FaxModem::BR57600 },
	{ "76800", FaxModem::BR76800 },
    };
    for (int i = (sizeof ( rates ) / sizeof ( rates [0])) -1; i >= 0; i--)
	if ((strcasecmp( cp ,  rates[i].name )==0) )
	    return (rates[i].br);
    return (FaxModem::BR0);
}
static BaudRate
getRate(const char* cp)
{
    BaudRate br = findRate(cp);
    if (br == FaxModem::BR0) {
	syslog(3 , "Unknown baud rate \"%s\", using 19200", cp);
	br = FaxModem::BR19200;			 
    }
    return (br);
}
static u_int
getFill(const char* cp)
{
    if ((strcasecmp( cp ,  "LSB2MSB" )==0) )
	return (2 );
    else if ((strcasecmp( cp ,  "MSB2LSB" )==0) )
	return (1 );
    else {
	syslog(3 , "Unknown fill order \"%s\"", cp);
        return ((u_int) -1);
    }
}
static FlowControl
getFlow(const char* cp)
{
    if ((strcasecmp( cp ,  "xonxoff" )==0) )
	return (FaxModem::FLOW_XONXOFF);
    else if ((strcasecmp( cp ,  "rtscts" )==0) )
	return (FaxModem::FLOW_RTSCTS);
    else if ((strcasecmp( cp ,  "none" )==0) )
	return (FaxModem::FLOW_NONE);
    else {
	syslog(3 , "Unknown flow control \"%s\", using xonxoff", cp);
	return (FaxModem::FLOW_XONXOFF);	 
    }
}
void
ModemConfig::setVolumeCmds(const fxStr& tag)
{
    u_int l = 0;
    for (int i = FaxModem::OFF; i <= FaxModem::HIGH; i++) {
	fxStr tmp = tag.token(l, " \t");		 
	setVolumeCmd[i] = parseATCmd(tmp);
    }
}
fxStr
ModemConfig::parseATCmd(const char* cp)
{
    fxStr cmd(cp);
    u_int pos = 0;
    while ((pos = cmd.next(pos, '<')) != cmd.length()) {
	u_int epos = pos+1;
	fxStr esc = cmd.token(epos, '>');
	esc.lowercase();
	char ecode;
	if (esc == "xon")
	    ecode = (0x80|0xf) ;
	else if (esc == "rts")
	    ecode = (0x80|0xe) ;
	else if (esc == "none")
	    ecode = (0x80|0xd) ;
	else if (esc == "")		 
	    ecode = '<';
	else {
	    BaudRate br = findRate(esc);
	    if (br == FaxModem::BR0) {
		syslog(3 , "Unknown AT escape code \"%s\"", (char*) esc);
		pos = epos;
		continue;
	    }
	    ecode = 0x80| ((int)( br )) ;
	}
	cmd.remove(pos, epos-pos);
	cmd.insert(ecode, pos);
    }
    return (cmd);
}
static const struct {
    const char*		 name;
    fxStr ModemConfig::* p;
} atcmds[] = {
    { "ModemAnswerCmd",			&ModemConfig::answerAnyCmd },
    { "ModemAnswerAnyCmd",		&ModemConfig::answerAnyCmd },
    { "ModemAnswerFaxCmd",		&ModemConfig::answerFaxCmd },
    { "ModemAnswerDataCmd",		&ModemConfig::answerDataCmd },
    { "ModemAnswerVoiceCmd",		&ModemConfig::answerVoiceCmd },
    { "ModemAnswerFaxBeginCmd",		&ModemConfig::answerFaxBeginCmd },
    { "ModemAnswerDataBeginCmd",	&ModemConfig::answerDataBeginCmd },
    { "ModemAnswerVoiceBeginCmd",	&ModemConfig::answerVoiceBeginCmd },
    { "ModemResetCmds",			&ModemConfig::resetCmds },
    { "ModemResetCmd",			&ModemConfig::resetCmds },
    { "ModemDialCmd",			&ModemConfig::dialCmd },
    { "ModemFlowControlCmd",		&ModemConfig::flowControlCmd },
    { "ModemSetupAACmd",		&ModemConfig::setupAACmd },
    { "ModemSetupDTRCmd",		&ModemConfig::setupDTRCmd },
    { "ModemSetupDCDCmd",		&ModemConfig::setupDCDCmd },
    { "ModemNoAutoAnswerCmd",		&ModemConfig::noAutoAnswerCmd },
    { "ModemEchoOffCmd",		&ModemConfig::echoOffCmd },
    { "ModemVerboseResultsCmd",		&ModemConfig::verboseResultsCmd },
    { "ModemResultCodesCmd",		&ModemConfig::resultCodesCmd },
    { "ModemOnHookCmd",			&ModemConfig::onHookCmd },
    { "ModemSoftResetCmd",		&ModemConfig::softResetCmd },
    { "ModemWaitTimeCmd",		&ModemConfig::waitTimeCmd },
    { "ModemCommaPauseTimeCmd",		&ModemConfig::pauseTimeCmd },
    { "ModemMfrQueryCmd",		&ModemConfig::mfrQueryCmd },
    { "ModemModelQueryCmd",		&ModemConfig::modelQueryCmd },
    { "ModemRevQueryCmd",		&ModemConfig::revQueryCmd },
    { "ModemSendBeginCmd",		&ModemConfig::sendBeginCmd },
    { "Class1Cmd",			&ModemConfig::class1Cmd },
    { "Class2Cmd",			&ModemConfig::class2Cmd },
    { "Class2BORCmd",			&ModemConfig::class2BORCmd },
    { "Class2RELCmd",			&ModemConfig::class2RELCmd },
    { "Class2CQCmd",			&ModemConfig::class2CQCmd },
    { "Class2AbortCmd",			&ModemConfig::class2AbortCmd },
    { "Class2CQQueryCmd",		&ModemConfig::class2CQQueryCmd },
    { "Class2DCCQueryCmd",		&ModemConfig::class2DCCQueryCmd },
    { "Class2TBCCmd",			&ModemConfig::class2TBCCmd },
    { "Class2CRCmd",			&ModemConfig::class2CRCmd },
    { "Class2PHCTOCmd",			&ModemConfig::class2PHCTOCmd },
    { "Class2BUGCmd",			&ModemConfig::class2BUGCmd },
    { "Class2LIDCmd",			&ModemConfig::class2LIDCmd },
    { "Class2DCCCmd",			&ModemConfig::class2DCCCmd },
    { "Class2DISCmd",			&ModemConfig::class2DISCmd },
    { "Class2DDISCmd",			&ModemConfig::class2DDISCmd },
    { "Class2CIGCmd",			&ModemConfig::class2CIGCmd },
    { "Class2PTSCmd",			&ModemConfig::class2PTSCmd },
    { "Class2SPLCmd",			&ModemConfig::class2SPLCmd },
    { "Class2PIECmd",			&ModemConfig::class2PIECmd },
    { "Class2NRCmd",			&ModemConfig::class2NRCmd },
    { "TagLineFont",			&ModemConfig::tagLineFontFile },
    { "TagLineFormat",			&ModemConfig::tagLineFmt },
};
static const struct {
    const char*		 name;
    u_int ModemConfig::* p;
} fillorders[] = {
    { "ModemRecvFillOrder",		&ModemConfig::recvFillOrder },
    { "ModemSendFillOrder",		&ModemConfig::sendFillOrder },
    { "ModemFrameFillOrder",		&ModemConfig::frameFillOrder },
};
static const struct {
    const char*		 name;
    u_int ModemConfig::* p;
} numbers[] = {
    { "ModemResetDelay",		&ModemConfig::resetDelay },
    { "ModemBaudRateDelay",		&ModemConfig::baudRateDelay },
    { "ModemMaxPacketSize",		&ModemConfig::maxPacketSize },
    { "ModemInterPacketDelay",		&ModemConfig::interPacketDelay },
    { "FaxT1Timer",			&ModemConfig::t1Timer },
    { "FaxT2Timer",			&ModemConfig::t2Timer },
    { "FaxT4Timer",			&ModemConfig::t4Timer },
    { "ModemDialResponseTimeout",	&ModemConfig::dialResponseTimeout },
    { "ModemAnswerResponseTimeout",	&ModemConfig::answerResponseTimeout },
    { "ModemPageStartTimeout",		&ModemConfig::pageStartTimeout },
    { "ModemPageDoneTimeout",		&ModemConfig::pageDoneTimeout },
    { "Class1TCFResponseDelay",		&ModemConfig::class1TCFResponseDelay },
    { "Class1SendPPMDelay",		&ModemConfig::class1SendPPMDelay },
    { "Class1SendTCFDelay",		&ModemConfig::class1SendTCFDelay },
    { "Class1TrainingRecovery",		&ModemConfig::class1TrainingRecovery },
    { "Class1RecvAbortOK",		&ModemConfig::class1RecvAbortOK },
    { "Class1FrameOverhead",		&ModemConfig::class1FrameOverhead },
    { "Class1RecvIdentTimer",		&ModemConfig::class1RecvIdentTimer },
    { "Class1TCFMaxNonZero",		&ModemConfig::class1TCFMaxNonZero },
    { "Class1TCFMinRun",		&ModemConfig::class1TCFMinRun },
    { "PercentGoodLines",		&ModemConfig::percentGoodLines },
    { "MaxConsecutiveBadLines",		&ModemConfig::maxConsecutiveBadLines },
};
fxBool
ModemConfig::parseItem(const char* tag, const char* value)
{
    int i;
    for (i = (sizeof ( atcmds ) / sizeof ( atcmds [0])) -1; i >= 0; i--)
	if ((strcasecmp( tag ,  atcmds[i].name )==0) ) {
	    (*this).*atcmds[i].p = parseATCmd(value);
	    return (((fxBool)1) );
	}
    for (i = (sizeof ( fillorders ) / sizeof ( fillorders [0])) -1; i >= 0 ; i--)
	if ((strcasecmp( tag ,  fillorders[i].name )==0) ) {
	    (*this).*fillorders[i].p = getFill(value);
	    return (((fxBool)1) );
	}
    for (i = (sizeof ( numbers ) / sizeof ( numbers [0])) -1; i >= 0 ; i--)
	if ((strcasecmp( tag ,  numbers[i].name )==0) ) {
	    (*this).*numbers[i].p = atoi(value);
	    return (((fxBool)1) );
	}
    fxBool recognized = ((fxBool)1) ;
    if ((strcasecmp( tag ,  "ModemType" )==0) )
	type = value;
    else if ((strcasecmp( tag ,  "ModemSetVolumeCmd" )==0) )
	setVolumeCmds(value);
    else if ((strcasecmp( tag ,  "ModemFlowControl" )==0) )
	flowControl = getFlow(value);
    else if ((strcasecmp( tag ,  "ModemMaxRate" )==0)  || (strcasecmp( tag ,  "ModemRate" )==0) )
	maxRate = getRate(value);
    else if ((strcasecmp( tag ,  "ModemWaitForConnect" )==0) )
	waitForConnect = getBoolean(value);
    else if ((strcasecmp( tag ,  "Class2RecvDataTrigger" )==0) )
	class2RecvDataTrigger = value;
    else if ((strcasecmp( tag ,  "Class2XmitWaitForXON" )==0) )
	class2XmitWaitForXON = getBoolean(value);
    else
	recognized = ((fxBool)0) ;
    return (recognized);
}
