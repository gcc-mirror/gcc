// Special g++ Options:
// Build don't link: 
// GROUPS passed old-abort
typedef unsigned long _G_clock_t;
typedef long _G_dev_t;
typedef long int _G_fpos_t;
typedef long _G_gid_t;
typedef unsigned long _G_ino_t;
typedef unsigned short _G_mode_t;
typedef short _G_nlink_t;
typedef long _G_off_t;
typedef long _G_pid_t;
typedef int _G_ptrdiff_t;
typedef int   _G_sigset_t;
typedef unsigned int _G_size_t;
typedef long _G_time_t;
typedef long _G_uid_t;
typedef unsigned int _G_wchar_t;
typedef int _G_ssize_t;
typedef double * _G_va_list;
class ostream; class streambuf; class backupbuf;
extern "C" int __underflow(streambuf*);
extern "C" int __overflow(streambuf*, int);
typedef _G_off_t streamoff;
typedef _G_off_t streampos;  
typedef unsigned long __fmtflags;
typedef unsigned char __iostate;
struct _ios_fields {  
    streambuf *_strbuf;
    ostream* _tie;
    int _width;
    __fmtflags _flags;
    _G_wchar_t _fill;
    __iostate _state;
    __iostate _exceptions;
    int _precision;
};
enum state_value {
    _good = 0 ,
    _eof = 1 ,
    _fail = 2 ,
    _bad = 4  };
enum open_mode {
    input = 1 ,
    output = 2 ,
    atend = 4 ,
    append = 8  };
class ios : public _ios_fields {
  public:
    typedef __fmtflags fmtflags;
    typedef int iostate;
    typedef int openmode;
    enum io_state {
	goodbit = 0 ,
	eofbit = 1 ,
	failbit = 2 ,
	badbit = 4  };
    enum open_mode {
	in = 1 ,
	out = 2 ,
	ate = 4 ,
	app = 8 ,
	trunc = 16 ,
	nocreate = 32 ,
	noreplace = 64 ,
	bin = 128  };
    enum seek_dir { beg, cur, end};
    enum { skipws=01, left=02, right=04, internal=010,
	   dec=020, oct=040, hex=0100,
	   showbase=0200, showpoint=0400, uppercase=01000, showpos=02000,
	   scientific=04000, fixed=010000, unitbuf=020000, stdio=040000,
	   dont_close=0x80000000  
	   };
    enum {  
	basefield=dec+oct+hex,
	floatfield = scientific+fixed,
	adjustfield = left+right+internal
    };
    ostream* tie() const { return _tie; }
    ostream* tie(ostream* val) { ostream* save=_tie; _tie=val; return save; }
    _G_wchar_t fill() const { return (_G_wchar_t)_fill; }
    _G_wchar_t fill(_G_wchar_t newf)
	{_G_wchar_t oldf = (_G_wchar_t)_fill; _fill = (char)newf; return oldf;}
    fmtflags flags() const { return _flags; }
    fmtflags flags(fmtflags new_val) {
	fmtflags old_val = _flags; _flags = new_val; return old_val; }
    int precision() const { return _precision; }
    int precision(int newp) {
	unsigned short oldp = _precision; _precision = (unsigned short)newp;
	return oldp; }
    fmtflags setf(fmtflags val) {
	fmtflags oldbits = _flags;
	_flags |= val; return oldbits; }
    fmtflags setf(fmtflags val, fmtflags mask) {
	fmtflags oldbits = _flags;
	_flags = (_flags & ~mask) | (val & mask); return oldbits; }
    fmtflags unsetf(fmtflags mask) {
	fmtflags oldbits = _flags & mask;
	_flags &= ~mask; return oldbits; }
    int width() const { return _width; }
    int width(int val) { int save = _width; _width = val; return save; }
    void _throw_failure() { }
    streambuf* rdbuf() const { return _strbuf; }
    void clear(iostate state = 0) {
	_state = _strbuf ? state : state|badbit;
	if (_state & _exceptions) _throw_failure(); }
    void set(iostate flag) { _state |= flag;
	if (_state & _exceptions) _throw_failure(); }
    int good() const { return _state == 0; }
    int eof() const { return _state & ios::eofbit; }
    int fail() const { return _state & (ios::badbit|ios::failbit); }
    int bad() const { return _state & ios::badbit; }
    iostate rdstate() const { return _state; }
    operator void*() const { return fail() ? (void*)0 : (void*)(-1); }
    int operator!() const { return fail(); }
    iostate exception(iostate enable) {
	iostate old = _exceptions; _exceptions = enable;
	if (_state & _exceptions) _throw_failure();
	return old; }
    static int sync_with_stdio(int on);
    static void sync_with_stdio() { sync_with_stdio(1); }
    void unset(state_value flag) { _state &= ~flag; }
    void close();
    int is_open();
    int readable();
    int writable();
  protected:
    ios(streambuf* sb = 0, ostream* tie = 0);
    virtual ~ios();
    void init(streambuf* sb) { _state=0; _strbuf=sb; }
};
typedef ios::seek_dir _seek_dir;
class streammarker {
    friend class streambuf;
    friend int __underflow(streambuf*);
    struct streammarker *_next;   
    streambuf *_sbuf;  
    streampos _spos;  
    void set_streampos(streampos sp) { _spos = sp; }
    void set_offset(int offset) { _pos = offset; _spos = (streampos)(-2); }
    int _pos;
  public:
    streammarker(streambuf *sb);
    ~streammarker();
    int saving() { return  _spos == -2; }
    int delta(streammarker&);
    int delta();
};
struct __streambuf {
    int _flags;		 
    char* _gptr;	 
    char* _egptr;	 
    char* _eback;	 
    char* _pbase; // ERROR - inacessible
    char* _pptr;  // ERROR - inacessible	 
    char* _epptr;	 
    char* _base;	 
    char* _ebuf;	 
    struct streambuf *_chain;
    friend class streammarker;
    char *_other_gbase;  
    char *_aux_limit;   
    char *_other_egptr;  
    streammarker *_markers;
    unsigned short _cur_column;
    char _unused;
    char _shortbuf[1];
};
extern unsigned __adjust_column(unsigned start, const char *line, int count);
struct streambuf : private __streambuf {
    friend class ios;
    friend class istream;
    friend class ostream;
    friend class streammarker;
    friend int __underflow(streambuf*);
  protected:
    static streambuf* _list_all;  
    streambuf*& xchain() { return _chain; }
    void _un_link();
    void _link_in();
    char* gptr() const { return _gptr; }
    char* pptr() const { return _pptr; }
    char* egptr() const { return _egptr; }
    char* epptr() const { return _epptr; }
    char* pbase() const { return _pbase; }
    char* eback() const { return _eback; }
    char* base() const { return _base; }
    char* ebuf() const { return _ebuf; }
    int blen() const { return _ebuf - _base; }
    void xput_char(char c) { *_pptr++ = c; }
    int xflags() { return _flags; }
    int xflags(int f) { int fl = _flags; _flags = f; return fl; }
    void xsetflags(int f) { _flags |= f; }
    void xsetflags(int f, int mask) { _flags = (_flags & ~mask) | (f & mask); }
    void gbump(int n) { _gptr += n; }
    void pbump(int n) { _pptr += n; }
    void setb(char* b, char* eb, int a=0);
    void setp(char* p, char* ep) { _pbase=_pptr=p; _epptr=ep; }
    void setg(char* eb, char* g, char *eg) { _eback=eb; _gptr=g; _egptr=eg; }
    char *shortbuf() { return _shortbuf; }
    int in_backup() { return _flags & 0x100 ; }
    char *Gbase() { return in_backup() ? _other_gbase : _eback; }
    char *eGptr() { return in_backup() ? _other_egptr : _egptr; }
    char *Bbase() { return in_backup() ? _eback : _other_gbase; }
    char *Bptr() { return _aux_limit; }
    char *eBptr() { return in_backup() ? _egptr : _other_egptr; }
    char *Nbase() { return _other_gbase; }
    char *eNptr() { return _other_egptr; }
    int have_backup() { return _other_gbase != __null ; }
    int have_markers() { return _markers != __null ; }
    int _least_marker();
    void switch_to_main_get_area();
    void switch_to_backup_area();
    void free_backup_area();
    void unsave_markers();  
    int put_mode() { return _flags & 0x800 ; }
    int switch_to_get_mode();
    streambuf(int flags=0);
  public:
    static int flush_all();
    static void flush_all_linebuffered();  
    virtual int underflow() = 0;  
    virtual int overflow(int c = (-1) ) = 0;  
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streampos seekpos(streampos pos, int mode = ios::in|ios::out);
    int seekmark(streammarker& mark, int delta = 0);
    int sputbackc(char c);
    int sungetc();
    virtual ~streambuf();
    int unbuffered() { return _flags & 2  ? 1 : 0; }
    int linebuffered() { return _flags & 0x200  ? 1 : 0; }
    void unbuffered(int i)
	{ if (i) _flags |= 2 ; else _flags &= ~2 ; }
    void linebuffered(int i)
	{ if (i) _flags |= 0x200 ; else _flags &= ~0x200 ; }
    int allocate() {  
	if (base() || unbuffered()) return 0;
	else return doallocate(); }
    void allocbuf() { if (base() == __null ) doallocbuf(); }
    void doallocbuf();
    virtual int sync();
    virtual int pbackfail(int c);
    virtual streambuf* setbuf(char* p, int len);
    int in_avail() { return _egptr - _gptr; }
    int out_waiting() { return _pptr - _pbase; }
    virtual int xsputn(const char* s, int n);
    int sputn(const char* s, int n) { return xsputn(s, n); }
    int padn(char pad, int n);  
    virtual int xsgetn(char* s, int n);
    int sgetn(char* s, int n) { return xsgetn(s, n); }
    int ignore(int);
    virtual int get_column();
    virtual int set_column(int);
    long sgetline(char* buf, _G_size_t n, char delim, int putback_delim);
    int sbumpc() {
	if (_gptr >= _egptr && __underflow(this) == (-1) ) return (-1) ;
	else return *(unsigned char*)_gptr++; }
    int sgetc() {
	if (_gptr >= _egptr && __underflow(this) == (-1) ) return (-1) ;
	else return *(unsigned char*)_gptr; }
    int snextc() {
	if (_gptr >= _egptr && __underflow(this) == (-1) ) return (-1) ;
	return _gptr++, sgetc(); }
    int sputc(int c) {
	if (_pptr >= _epptr) return __overflow(this, (unsigned char)c);
	else return *_pptr++ = c, (unsigned char)c; }
    void stossc() { if (_gptr < _egptr) _gptr++; }
    int vscan(char const *fmt0, _G_va_list ap, ios* stream = __null );
    int scan(char const *fmt0 ...);
    int vform(char const *fmt0, _G_va_list ap);
    int form(char const *fmt0 ...);
};
class backupbuf : public streambuf {
    friend class streammarker;
  protected:
    backupbuf(int flags=0) : streambuf(flags| 0x4000 ) { }
  public:
    virtual int pbackfail(int c);
    virtual int underflow();
    virtual int overflow(int c = (-1) );
};
struct __file_fields {
    short _fileno;
    int _blksize;
    _G_fpos_t _offset;
};
class filebuf : public backupbuf {
  protected:
    struct __file_fields _fb;
    void init();
  public:
    static const int openprot;  
    filebuf();
    filebuf(int fd);
    filebuf(int fd, char* p, int len);
    ~filebuf();
    filebuf* attach(int fd);
    filebuf* open(const char *filename, const char *mode);
    filebuf* open(const char *filename, ios::openmode mode, int prot = 0664);
    virtual int underflow();
    virtual int overflow(int c = (-1) );
    int is_open() const { return _fb._fileno >= 0; }
    int fd() const { return is_open() ? _fb._fileno : (-1) ; }
    filebuf* close();
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streambuf* setbuf(char* p, int len);
    int xsputn(const char* s, int n);
    int xsgetn(char* s, int n);
    virtual int sync();
  protected:  
    int is_reading() { return eback() != egptr(); }
    char* cur_ptr() { return is_reading() ?  gptr() : pptr(); }
    char* file_ptr() { return eGptr(); }
    int do_write(const char *data, int to_do);
    int do_flush() { return do_write(_pbase, _pptr-_pbase); }// ERROR - 
    virtual _G_ssize_t sys_read(char* buf, _G_size_t size);
    virtual _G_fpos_t sys_seek(_G_fpos_t, _seek_dir);
    virtual _G_ssize_t sys_write(const void*, long);
    virtual int sys_stat(void*);  
    virtual int sys_close();
};
inline ios::ios(streambuf* sb  , ostream* tie  ) {
		_state = sb ? ios::goodbit : ios::badbit; _exceptions=0;
		_strbuf=sb; _tie = tie; _width=0; _fill=' ';
		_flags=ios::skipws|ios::dec; _precision=6; }
inline ios::~ios() {
    if (!(_flags & (unsigned int)ios::dont_close)) delete _strbuf; }
class istream; class ostream;
typedef ios& (*__manip)(ios&);
typedef istream& (*__imanip)(istream&);
typedef ostream& (*__omanip)(ostream&);
extern istream& ws(istream& ins);
extern ostream& flush(ostream& outs);
extern ostream& endl(ostream& outs);
extern ostream& ends(ostream& outs);
class ostream : virtual public ios
{
    void do_osfx();
  public:
    ostream() { }
    ostream(streambuf* sb, ostream* tied=__null );
    int opfx() {
	if (!good()) return 0; else { if (_tie) _tie->flush(); return 1;} }
    void osfx() { if (flags() & (ios::unitbuf|ios::stdio))
		      do_osfx(); }
    streambuf* ostreambuf() const { return _strbuf; }
    ostream& flush();
    ostream& put(char c) { _strbuf->sputc(c); return *this; }
    ostream& put(unsigned char c) { return put((char)c); }
    ostream& write(const char *s, int n);
    ostream& write(const unsigned char *s, int n) { return write((const char*)s, n);}
    ostream& put(signed char c) { return put((char)c); }
    ostream& write(const signed char *s, int n) { return write((const char*)s, n);}
    ostream& write(const void *s, int n) { return write((const char*)s, n);}
    ostream& seekp(streampos);
    ostream& seekp(streamoff, _seek_dir);
    streampos tellp();
    ostream& form(const char *format ...);
    ostream& vform(const char *format, _G_va_list args);
    ostream& operator<<(char c);
    ostream& operator<<(unsigned char c) { return (*this) << (char)c; }
    ostream& operator<<(signed char c) { return (*this) << (char)c; }
    ostream& operator<<(const char *s);
    ostream& operator<<(const unsigned char *s)
	{ return (*this) << (const char*)s; }
    ostream& operator<<(const signed char *s)
	{ return (*this) << (const char*)s; }
    ostream& operator<<(void *p);
    ostream& operator<<(int n);
    ostream& operator<<(unsigned int n);
    ostream& operator<<(long n);
    ostream& operator<<(unsigned long n);
    ostream& operator<<(long long n);
    ostream& operator<<(unsigned long long n);
    ostream& operator<<(short n) {return operator<<((int)n);}
    ostream& operator<<(unsigned short n) {return operator<<((unsigned int)n);}
    ostream& operator<<(double n);
    ostream& operator<<(float n) { return operator<<((double)n); }
    ostream& operator<<(__omanip func) { return (*func)(*this); }
    ostream& operator<<(__manip func) {(*func)(*this); return *this;}
    ostream& operator<<(streambuf*);
};
class istream : virtual public ios
{
    _G_ssize_t _gcount;
    int _skip_ws();
  public:
    istream() { _gcount = 0; }
    istream(streambuf* sb, ostream*tied=__null );
    streambuf* istreambuf() const { return _strbuf; }
    istream& get(char* ptr, int len, char delim = '\n');
    istream& get(unsigned char* ptr, int len, char delim = '\n')
	{ return get((char*)ptr, len, delim); }
    istream& get(char& c);
    istream& get(unsigned char& c) { return get((char&)c); }
    istream& getline(char* ptr, int len, char delim = '\n');
    istream& getline(unsigned char* ptr, int len, char delim = '\n')
	{ return getline((char*)ptr, len, delim); }
    istream& get(signed char& c)  { return get((char&)c); }
    istream& get(signed char* ptr, int len, char delim = '\n')
	{ return get((char*)ptr, len, delim); }
    istream& getline(signed char* ptr, int len, char delim = '\n')
	{ return getline((char*)ptr, len, delim); }
    istream& read(char *ptr, int n);
    istream& read(unsigned char *ptr, int n) { return read((char*)ptr, n); }
    istream& read(signed char *ptr, int n) { return read((char*)ptr, n); }
    istream& read(void *ptr, int n) { return read((char*)ptr, n); }
    istream& get(streambuf& sb, char delim = '\n');
    istream& gets(char **s, char delim = '\n');
    int ipfx(int need) {
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie && (need == 0 || rdbuf()->in_avail() < need)) _tie->flush();
	if (!need && (flags() & ios::skipws)) return _skip_ws();
	return 1;
    }
    int ipfx0() {  
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie) _tie->flush();
	if (flags() & ios::skipws) return _skip_ws();
	return 1;
    }
    int ipfx1() {  
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie && rdbuf()->in_avail() == 0) _tie->flush();
	return 1;
    }
    int get() { if (!ipfx1()) return (-1) ;
		int ch = _strbuf->sbumpc();
		if (ch == (-1) ) set(ios::eofbit);
		return ch; }
    int peek() { if (!ipfx1()) return (-1) ;
		int ch = _strbuf->sgetc();
		if (ch == (-1) ) set(ios::eofbit);
		return ch; }
    _G_ssize_t gcount() { return _gcount; }
    istream& ignore(int n=1, int delim = (-1) );
    istream& seekg(streampos);
    istream& seekg(streamoff, _seek_dir);
    streampos tellg();
    istream& putback(char ch) {
	if (good() && _strbuf->sputbackc(ch) == (-1) ) clear(ios::badbit);
	return *this;}
    istream& unget() {
	if (good() && _strbuf->sungetc() == (-1) ) clear(ios::badbit);
	return *this;}
    istream& scan(const char *format ...);
    istream& vscan(const char *format, _G_va_list args);
    istream& unget(char ch) { return putback(ch); }
    int skip(int i);
    istream& operator>>(char*);
    istream& operator>>(unsigned char* p) { return operator>>((char*)p); }
    istream& operator>>(signed char*p) { return operator>>((char*)p); }
    istream& operator>>(char& c);
    istream& operator>>(unsigned char& c) {return operator>>((char&)c);}
    istream& operator>>(signed char& c) {return operator>>((char&)c);}
    istream& operator>>(int&);
    istream& operator>>(long&);
    istream& operator>>(long long&);
    istream& operator>>(short&);
    istream& operator>>(unsigned int&);
    istream& operator>>(unsigned long&);
    istream& operator>>(unsigned long long&);
    istream& operator>>(unsigned short&);
    istream& operator>>(float&);
    istream& operator>>(double&);
    istream& operator>>( __manip func) {(*func)(*this); return *this;}
    istream& operator>>(__imanip func) { return (*func)(*this); }
    istream& operator>>(streambuf*);
};
class iostream : public istream, public ostream
{
    _G_ssize_t _gcount;
  public:
    iostream() { _gcount = 0; }
    iostream(streambuf* sb, ostream*tied=__null );
};
extern istream cin;
extern ostream cout, cerr, clog;  
struct Iostream_init { } ;   
inline ios& dec(ios& i)
{ i.setf(ios::dec, ios::dec|ios::hex|ios::oct); return i; }
inline ios& hex(ios& i)
{ i.setf(ios::hex, ios::dec|ios::hex|ios::oct); return i; }
inline ios& oct(ios& i)
{ i.setf(ios::oct, ios::dec|ios::hex|ios::oct); return i; }
extern char* form(const char*, ...);
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
typedef double *__gnuc_va_list;
extern "C" {
   typedef struct {
	int		 __cnt;
	unsigned char	*__ptr;
	unsigned char	*__base;
	unsigned short	 __flag;
	unsigned char 	 __fileL;		 
	unsigned char 	 __fileH;		 
   } FILE;
   typedef struct {
	int		 __cnt;
	unsigned char	*__ptr;
	unsigned char	*__base;
	unsigned short	 __flag;
	unsigned char 	 __fileL;		 
	unsigned char 	 __fileH;		 
	unsigned char	*__bufendp;	 
	unsigned char	 __smbuf[8 ];  
   } _FILEX;
     typedef unsigned int size_t;
   typedef long int fpos_t;
     typedef double *__va_list;
   extern FILE __iob[];
     extern int remove(const char *);
     extern int rename(const char *, const char *);
     extern FILE *tmpfile(void);
     extern char *tmpnam(char *);
     extern int fclose(FILE *);
     extern int fflush(FILE *);
     extern FILE *fopen(const char *, const char *);
     extern FILE *freopen(const char *, const char *, FILE *);
     extern void setbuf(FILE *, char *);
     extern int setvbuf(FILE *, char *, int, size_t);
     extern int fprintf(FILE *, const char *, ...);
     extern int fscanf(FILE *, const char *,...);
     extern int printf(const char *,...);
     extern int scanf(const char *,...);
     extern int sprintf(char *, const char *,...);
     extern int sscanf(const char *, const char *,...);
     extern int vprintf(const char *, __va_list);
     extern int vfprintf(FILE *, const char *, __va_list);
     extern int vsprintf(char *, const char *, __va_list);
     extern int fgetc(FILE *);
     extern char *fgets(char *, int, FILE *);
     extern int fputc(int, FILE *);
     extern int fputs(const char *, FILE *);
     extern int getc(FILE *);
     extern int getchar(void);
     extern char *gets(char *);
     extern int putc(int, FILE *);
     extern int putchar(int);
     extern int puts(const char *);
     extern int ungetc(int, FILE *);
     extern int fgetpos(FILE *, fpos_t *);
     extern int fseek(FILE *, long int, int);
     extern int fsetpos(FILE *, const fpos_t *);
     extern long int ftell(FILE *);
     extern void rewind(FILE *);
     extern void clearerr(FILE *);
     extern int feof(FILE *);
     extern int ferror(FILE *);
     extern void perror(const char *);
       extern size_t fread(void *, size_t, size_t, FILE *);
       extern size_t fwrite(const void *, size_t, size_t, FILE *);
     extern int __flsbuf(unsigned char, FILE *);
     extern int __filbuf(FILE *);
     extern int fileno(FILE *);
     extern FILE *fdopen(int, const char *);
     extern int getw(FILE *);
     extern int putw(int, FILE *);
     extern int pclose(FILE *);
     extern FILE *popen(const char *, const char *);
     extern char *tempnam(const char *, const char *);
     extern char *ctermid(char *);
     extern char *cuserid(char *);
     extern int nl_fprintf(FILE *, const char * ,...);
     extern int nl_fscanf(FILE *, const char * ,...);
     extern int nl_printf(const char * ,...);
     extern int nl_scanf(const char * ,...);
     extern int nl_sprintf(char *, const char * ,...);
     extern int nl_sscanf(const char *, const char * ,...);
   extern unsigned char *__bufendtab[];
}
extern "C" {
   extern int __nl_char_size;
   typedef struct {
	int quot;	 
	int rem;	 
   } div_t;
   typedef struct {
	long int quot;	 
	long int rem;	 
   } ldiv_t;
     typedef unsigned int wchar_t;
       extern double atof(const char *);
     extern int atoi(const char *);
     extern long int atol(const char *);
     extern double strtod(const char *, char **);
     extern long int strtol(const char *, char **, int);
     extern unsigned long int strtoul(const char *, char **, int);
     extern int rand(void);
     extern void srand(unsigned int);
     extern int atexit(void (*) (void));
     extern void exit(int);
     extern char *getenv(const char *);
     extern int system(const char *);
       inline int abs(int d) { return (d>0)?d:-d; }
     extern div_t div(int, int);
     extern ldiv_t ldiv(long int, long int);
     extern long int labs(long int);
     extern int mblen(const char *, size_t);
     extern int mbtowc(wchar_t *, const char *, size_t);
     extern int wctomb(char *, wchar_t);
     extern size_t mbstowcs(wchar_t *, const char *, size_t);
     extern size_t wcstombs(char *, const wchar_t *, size_t);
     extern void free(void *);
     extern void qsort(void *, size_t, size_t, int (*)(const void *, const void *));
       extern void abort(void);
       extern void *bsearch(const void *, const void *, size_t, size_t, int (*) (const void *, const void *));
       extern void *calloc(size_t, size_t);
       extern void *malloc(size_t);
       extern void *realloc(void *, size_t);
     extern void setkey(const char *);
     extern void lcong48( unsigned short [] );
     extern double wcstod( const wchar_t *, wchar_t ** );
     extern long wcstol( const wchar_t *, wchar_t **, int );
     extern unsigned long wcstoul( const wchar_t *, wchar_t **, int );
     extern double drand48(void);
     extern double erand48(unsigned short []);
     extern long jrand48(unsigned short []);
     extern long lrand48(void);
     extern long mrand48(void);
     extern long nrand48(unsigned short []);
     extern void srand48(long);
     extern unsigned short *seed48(unsigned short []);
     extern int putenv(const char *);
    extern int clearenv(void);
    extern int getopt(int, char * const [], const char *);
    extern char *getpass(const char *);
  extern char *optarg;
  extern int optind;
  extern int opterr;
  struct mallinfo  {
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
  };	
     typedef struct {
       unsigned int word1, word2, word3, word4;
     } long_double;
extern "C" {
     typedef long gid_t;
     typedef long uid_t;
   struct passwd {
	char	*pw_name;
	char 	*pw_passwd;
	uid_t	pw_uid;
	gid_t	pw_gid;
	char 	*pw_age;
	char	*pw_comment;
	char	*pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
	long	pw_audid;
	int	pw_audflg;
   };
     extern struct passwd *getpwuid(uid_t);
     extern struct passwd *getpwnam(const char *);
   struct s_passwd {
       char    *pw_name;
       char    *pw_passwd;
       char    *pw_age;
       long     pw_audid;
       int     pw_audflg;
   };
   struct comment {
	char	*c_dept;
	char	*c_name;
	char	*c_acct;
	char	*c_bin;
   };
     extern void setpwent(void);
     extern void endpwent(void);
     extern struct passwd *getpwent(void);
     extern struct passwd *fgetpwent(FILE *);
     extern struct s_passwd *getspwent(void);
     extern struct s_passwd *getspwuid(int);
     extern struct s_passwd *getspwaid(int);
     extern struct s_passwd *getspwnam(char *);
     extern struct s_passwd *fgetspwent(FILE *);
}
extern int errno;
     extern "C" {
         extern int errno;
     }
extern "C" {
    extern void perror(const char*);
    extern int sys_nerr;
    extern char *sys_errlist[];
    extern char *strerror (int);
}
     extern const char *fcvt(double, size_t, int *, int *);
     extern char *gcvt(double, size_t, char *);
     extern char *ecvt(double, size_t, int *, int *);
     extern char *nl_gcvt(double, size_t, char *, int);
     extern char *_ldecvt(long_double, size_t, int *, int *);
     extern char *_ldfcvt(long_double, size_t, int *, int *);
     extern char *_ldgcvt(long_double, size_t, char *);
     extern int getpw(int, char *);
     extern long a64l(const char *);
     extern char *l64a(long);
     extern void l3tol(long *, const char *, int);
     extern void ltol3(char *, const long *, int);
     extern char *getmsg(int, int, int, char *, int);
     extern double nl_atof(const char *, int);
     extern double nl_strtod(const char *, char **, int);
     extern char *ltostr(long, int);
     extern char *ultostr(unsigned long, int);
     extern char *ltoa(long);
     extern char *ultoa(unsigned long);
     extern void memorymap(int);
     extern struct mallinfo mallinfo(void);
     extern int mallopt(int, int);
}
typedef void * POINTER;
typedef FILE * FILE_PTR;
typedef void (*DISPLAYER)   (POINTER obj, FILE_PTR fp, FILE_PTR errorFile)  ;
typedef void (*DESTROYER)   (POINTER obj, FILE_PTR errorFile)  ;
typedef void (*RCOUNT_INC)  (POINTER obj, FILE_PTR errorFile)  ;
typedef int  (*COMPARE)     (POINTER obj1, POINTER obj2)  ;
typedef	unsigned long (*HASH_ADDR)  (POINTER obj, FILE_PTR errorFile)  ;
typedef	unsigned long (*HASH_SKIP)  (POINTER obj, FILE_PTR errorFile)  ;
typedef POINTER
	(*ACCESS_FCT)  (POINTER target, POINTER sample, FILE_PTR errorFile)  ;
typedef POINTER DATA_PTR;
     typedef long dev_t;		 
     typedef unsigned long ino_t;	 
     typedef unsigned short mode_t; 	 
     typedef short nlink_t;		 
     typedef long off_t;		 
     typedef long pid_t;		 
     typedef long time_t;		 
      typedef int ssize_t;		 
     typedef unsigned short __site_t;	 
     typedef unsigned short __cnode_t;	 
     typedef unsigned long clock_t;	 
      typedef long key_t;		 
   typedef unsigned short __ushort;	 
   typedef long	__daddr_t;		 
   typedef char *__caddr_t;		 
   typedef long __swblk_t;
   typedef unsigned char	u_char;	    
   typedef unsigned short	u_short;    
   typedef unsigned int		u_int;      
   typedef unsigned long	u_long;     
   typedef unsigned int		uint;	    
   typedef unsigned short	ushort;	    
   typedef __swblk_t		swblk_t;
   typedef __daddr_t		daddr_t;
   typedef __site_t		site_t;
   typedef __cnode_t		cnode_t;
     typedef __caddr_t		caddr_t;    
   typedef long			paddr_t;
   typedef short		cnt_t;
   typedef unsigned int		space_t;
   typedef unsigned int    	prot_t;
   typedef unsigned long        cdno_t;
   typedef unsigned short	use_t;
   typedef struct _physadr { int r[1]; } *physadr;
   typedef struct _quad { long val[2]; } quad;
     typedef short cpu_t;
     typedef struct label_t {
	int	lbl_rp;
       	int	lbl_sp;
       	int	lbl_s[17];
       	int	lbl_ss[1];
	double	lbl_sf[4];
     } label_t;
   typedef char *dm_message;
      typedef long	aid_t;
   typedef pid_t		sid_t;	    
     typedef long fd_mask;
     typedef struct fd_set {
       fd_mask fds_bits[ (((       2048   )+((  (sizeof(fd_mask) * 8)      )-1))/(  (sizeof(fd_mask) * 8)      )) ];
     } fd_set;
POINTER malloc  (size_t size )  ;
POINTER calloc  (size_t n , size_t size )  ;
void  free  (POINTER c )  ;
void mem_report  (void )  ;
typedef  int         BOOLEAN;
typedef enum
{
  eNoErrors			= -1,
  eUnknownProcessingError       = 0,
  eAlgorithmFailure		= 100,
  eAlreadyLocked                = 200,
  eAttCount			= 300,
  eAttRedefinition		= 400,
  eBasicMatrixError		= 500,
  eBadAddress			= 600,
  eBadAttribute			= 700,
  eBadFileNumber		= 800,
  eBadMode			= 900,
  eBadVersion			= 1000,
  eColumnError			= 1100,
  eConstraintError		= 1200,
  eLBConstraintError		= 1300,
  eUBConstraintError		= 1400,
  eDatabaseError		= 1500,
  eDataExists			= 1600,
  eDataNotFound			= 1700,
  eDictError			= 1800,
  eDiffAtt			= 1900,
  eDomain			= 2000,
  eDuplicateAtt			= 2100,
  eDuplicateKey			= 2200,
  eElementMismatch		= 2300,
  eEmptyAtt			= 2400,
  eEmptyDict			= 2450,
  eEmptyLink			= 2500,
  eEmptyQueue			= 2600,
  eEmptyStack			= 2700,
  eEntityExists			= 2800,
  eEntityNotFound		= 2900,
  eEtaComputeError		= 3000,
  eEtaFileError			= 3100,
  eEtaInverseConsistency	= 3200,
  eFileExists			= 3300,
  eFileNotFound			= 3400,
  eFunctionMismatch		= 3500,
  eFunctionNotAvailable		= 3600,
  eFunctionNotFound		= 3700,
  eFunctionTypeConflict		= 3800,
  eGraphError			= 3900,
  eIllegalBMatrixOp		= 4000,
  eIllegalData			= 4100,
  eIllegalDeletion		= 4200,
  eIllegalDictOp		= 4300,
  eIllegalEtaFileOp		= 4400,
  eIllegalFunctionRequest	= 4500,
  eIllegalFunctionType		= 4600,
  eIllegalGraphOp		= 4700,
  eIllegalGraphType		= 4800,
  eIllegalIndex			= 4900,
  eIllegalInsertion		= 5000,
  eIllegalListOp		= 5100,
  eIllegalLPTokenOp		= 5200,
  eIllegalLPTokenType		= 5300,
  eIllegalMatrixOp		= 5400,
  eIllegalMatrixSize		= 5500,
  eIllegalMatrixState		= 5600,
  eIllegalMatrixType		= 5700,
  eIllegalNBMatrixOp		= 5800,
  eIllegalOperation		= 5900,
  eIllegalOption		= 6000,
  eIllegalOverload		= 6100,
  eIllegalSeek			= 6200,
  eIllegalType			= 6300,
  eIllegalVectorOp		= 6400,
  eIllegalVectorType		= 6500,
  eIllFormedADT			= 6600,
  eInconsistentData		= 6700,
  eInternalError		= 6800,
  eInUse			= 6900,
  eInvalidArgument		= 7000,
  eInvalidAtt			= 7100,
  eInvalidData			= 7200,
  eInvalidIndex			= 7300,
  eInvalidMatrixOp		= 7400,
  eInvalidOperation		= 7500,
  eInvalidOption		= 7600,
  eInvalidParam			= 7700,
  eInvalidVectorOp		= 7800,
  eKeyNotFound			= 7900,
  eLinkNotEmpty			= 8000,
  eListError			= 8100,
  eLockAccess			= 8200,
  eLockExists			= 8300,
  eLockNotFound			= 8400,
  eLockReadOnly			= 8500,
  eLockTrunc			= 8600,
  eLPTokenError			= 8700,
  eLPTokenMisuse		= 8800,
  eMatrixError			= 8900,
  eMatrixMismatch		= 9000,
  eMatrixMultiplicationError	= 9100,
  eMissingData			= 9200,
  eMysteryError			= 9300,
  eNameTooLong			= 9400,
  eNeverLocked			= 9500,
  eNoAccess			= 9600,
  eNoLockReq			= 9800,
  eNoMoreAtt			= 9900,
  eNoMoreElements		= 10000,
  eNonBasicMatrixError		= 10100,
  eNonInvertibleMatrix		= 10200,
  eNonsenseData			= 10300,
  eNoSuchKey			= 10400,
  eNotEnoughSpace		= 10500,
  eNotFetched			= 10600,
  eNotLocked			= 10700,
  eNotSupported			= 10800,
  eNullPointer			= 10900,
  eNullInternalPointer		= 11000,
  eNullCompare			= 11050,
  eNullData			= 11070,
  eNullDestroyer		= 11100,
  eNullDisplayer		= 11200,
  eNullFunction			= 11300,
  eRemove			= 11400,
  eRoundoff			= 11500,
  eQueueNotEmpty		= 11600,
  eSparseGraphError		= 11700,
  eStackError			= 11800,
  eStackNotEmpty		= 11900,
  eTLRSProceduralError		= 12000,
  eTooManyAtt			= 12100,
  eTooManyLocks			= 12200,
  eTypeConflict			= 12300,
  eUnexpectedType		= 12400,
  eUnixError			= 12500,
  eVectorMismatch		= 12600,
  eProcessingError		= 15000, 
  eActionKludge			= 15100,
  eNotImplemented		= 15200,
  eNotStable			= 15300,
  eNotReleased			= 15400,
  eBeingWorkedOn		= 15500
} eErrorState;
extern "C" {
extern	eErrorState	spider_errno;
extern	char		err_string[];
POINTER mallocate  (size_t n )  ;
size_t pagesize  (void )  ;
POINTER page_mallocate  (void )  ;
POINTER make_memory_token  (int size , int page_cnt, const char *name)  ;
POINTER mem_malloc  (POINTER mptr )  ;
void mem_free  (POINTER mptr , POINTER obj )  ;
const char *get_double_string  (char *buffer , double num )  ;
int spider_strcat  (const char *proc , char *str1 , const char *str2 , int maxlen )  ;
void init_errors  (void )  ;
void append_abort  (const char *proc )  ;
void append_message  (const char *proc , const char *msg )  ;
void set_errno  (eErrorState err )  ;
void spider_perror  (const char *proc , FILE_PTR errorFile )  ;
void append_not_implemented  (const char *proc , int *target_errno )  ;
const char *GetBooleanString  (BOOLEAN bool )  ;// ERROR - use of bool as identifier
double truncate_value  (double x , double epsilon )  ;
double log2  (double x )  ;
int ceil_log2  (double x )  ;
int floor_log2  (double x )  ;
}
extern "C" {
extern  char char80[], char128[], char160[], char512[];
extern  char char1000[], char2000[];
extern  char char320[];	 
BOOLEAN Get_EM_StderrFlag  (void )  ;
void Set_EM_StderrFlag  (BOOLEAN printToStderr )  ;
void CheckReleaseState  (eErrorState releaseState , const char *procName, 			  FILE_PTR errorFile )  ;
void PrintErrorString  (const char *errorString , FILE_PTR errorFile )  ;
const char *get_error_header  (eErrorState errorState )  ;
void print_error_header  (eErrorState errorState , FILE_PTR errorFile )  ;
void PrintErrorMessage  (eErrorState errorState , const char *message , 			  FILE_PTR errorFile )  ;
}
extern "C" {
extern short show_increment_rcount;
extern int errno;
extern int bmk_errno;
extern int ck_errno;
extern int db_errno;
extern int dict_errno;
extern int glh_errno;
extern int graph_errno;
extern int hash_errno;
extern int link_errno;
extern int lp_errno;
extern int matrix_errno;
extern int nbmk_errno;
extern int network_errno;
extern int queue_errno;
extern int set_kernel_errno;
extern int stack_errno;
}
extern "C" {
const char *spider_version_string  (void )  ;
int  spider_version  (void )  ;
const char *spider_errstring  (void )  ;
void append_perror  (const char *s )  ;
void unix_perror  (const char *s )  ;
}
typedef struct node a_dict_node, * DICT_NODE;
struct node {
	POINTER info;		 
	DICT_NODE left;		 
	DICT_NODE right;	 
	DICT_NODE prev;		 
	DICT_NODE next;		 
};
typedef struct tree a_dict, * DICT;
struct tree {
	int	  size;			 
	COMPARE   comp; 		 
	DICT_NODE header;		 
	DICT_NODE last_accessed;	 
	DICT_NODE list_head;		 
	DICT_NODE list_tail;		 
};
typedef enum { PREORDER = -1, INORDER, POSTORDER } traversal_t;
extern "C" {
void dict_set_nopages  (int desired_nopages )  ;
void dict_err  (void )  ;
const char *traversal_string  (traversal_t tr )  ;
DICT make_dictionary  (COMPARE cmp )  ;
int dict_insert  (POINTER item , DICT dict )  ;
int dict_delete  (POINTER item , DICT dict )  ;
POINTER dict_access  (POINTER item , DICT dict )  ;
POINTER dict_low  (DICT dict )  ;
POINTER dict_nlow  (DICT dict )  ;
POINTER dict_big  (DICT dict )  ;
POINTER dict_nbig  (DICT dict )  ;
POINTER dict_first  (DICT dict )  ;
POINTER dict_next  (DICT dict )  ;
POINTER dict_last  (DICT dict )  ;
POINTER dict_prev  (DICT dict )  ;
int clear_dictionary  (DICT dict , DESTROYER destroy , FILE *errorFile )  ;
int dict_size  (const a_dict *dict )  ;
POINTER dict_elem  (int index , DICT dict )  ;
int dict_compare  (DICT d1 , DICT d2 )  ;
DICT dict_copy  (DICT di , COMPARE cmp )  ;
int dict_change_compare  (DICT di , COMPARE cmp )  ;
int dict_height  (DICT di )  ;
void dict_print_stats  (FILE *fp , DICT di )  ;
void dict_print  (FILE *fp , DICT di , traversal_t tr , DISPLAYER display )  ;
const char *dict_version  (void )  ;
void init_dict_errors  (void )  ;
const char *dict_errstring  (void )  ;
void dict_append_perror  (const char *s )  ;
void dict_perror  (const char *s )  ;
extern int  dict_errno;
}
typedef struct q_item {
  struct q_item *next;
  POINTER data;
} a_q_item, *Q_ITEM;
typedef struct {
  int rcount;
  Q_ITEM first;
  Q_ITEM last;
  int count;
} a_queue, *QUEUE;
extern "C" {
void queue_err  (void)  ;
void queue_set_nopages  (int desired_nopages)  ;
QUEUE make_queue  (void )  ;
POINTER queue_look  (const a_queue * Q )  ;
POINTER queue_last  (const a_queue * Q )  ;
POINTER queue_remove  (QUEUE Q )  ;
int queue_append  (QUEUE Q , POINTER data )  ;
int queue_size  (const a_queue * Q )  ;
void queue_destroy  (POINTER ptr , FILE_PTR errorFile )  ;
void IncrementQRCount  (POINTER ptr , FILE_PTR errorFile )  ;
const char *queue_version  (void )  ;
void init_queue_errors  (void )  ;
const char *queue_errstring  (void )  ;
void queue_append_perror  (const char *s )  ;
void queue_perror  (const char *s )  ;
extern int queue_errno;
}
typedef struct s_item {
  struct s_item *next;
  POINTER data;
} a_s_item, *S_ITEM;
typedef struct stack {
  int rcount;
  S_ITEM top;
  int count;
} a_stack, *STACK;
extern "C" {
void stack_err  (void)  ;
void stack_set_nopages  (int desired_nopages)  ;
STACK make_stack  (void )  ;
POINTER stack_look  (const a_stack * S )  ;
POINTER stack_pop  (STACK S )  ;
int stack_push  (STACK S , POINTER data )  ;
int stack_size  (const a_stack * S )  ;
void stack_destroy  (POINTER ptr , FILE_PTR errorFile )  ;
void IncrementSRCount  (POINTER ptr , FILE_PTR errorFile )  ;
const char *stack_version  (void )  ;
void init_stack_errors  (void )  ;
const char *stack_errstring  (void )  ;
void stack_append_perror  (const char *s )  ;
void stack_perror  (const char *s )  ;
extern int stack_errno;
}
typedef const char *(*STRINGER)(const void *ptr);
int default_compare(const void *p1, const void *p2);
void default_displayer(const void *ptr, FILE *fp, FILE *errorFile);
const char *default_stringer(const void *ptr);
const char *object_stringer(const void *ptr);
class object {
	static long current_id;
	static long next_id();
protected:
	void display2(ostream &fp = cout) const;
	long id;
public:
	object()                  { id = next_id(); }
	object(const object &obj) { id = next_id(); }
	virtual ~object()
	{
		if (799  >=  999 ) cout <<   "~object() executing..."  << '\n'; ;
	}
	object & operator = (const object & obj)
	{
		return( *this );
	}
	virtual int compare(const void *p2) const;
	virtual void display(ostream & fp = cout) const
	{
		display2(fp);
	}
	operator char *() const;
};
ostream & operator << (ostream & fp, object & obj);
int operator < (object & obj1, object & obj2);
int operator <= (object & obj1, object & obj2);
int operator > (object & obj1, object & obj2);
int operator >= (object & obj1, object & obj2);
int operator == (object & obj1, object & obj2);
int operator != (object & obj1, object & obj2);
void null_destroy(void *p, FILE *errorFile);
void object_destroy(void *p, FILE *errorFile);
int  object_compare(const void *p1, const void *p2);
void object_display(const void *p, FILE *fp, FILE *errorFile);
class dict : public object {
	class internal {
	public:
		DICT di;
		int errno;
		COMPARE compare_f;
		DISPLAYER display_f;
		DESTROYER destroy_f;
		STRINGER string_f;
		int count;
		void append_error(int expr, const char *proc)
		{
			if (expr) dict_append_perror(proc);
			errno = dict_errno;
		}
		void obj_error(int expr, const char *proc, const void *obj)
		{
			if (expr) {
				dict_append_perror((*string_f)(obj));
				dict_append_perror(proc);
			}
			errno = dict_errno;
		}
		internal(COMPARE cmp, DESTROYER destroyer,
			 DISPLAYER displayer, STRINGER str_f)
		{// ERROR - candidate for bad call
			if (799  >=  800 ) cout <<  "Creating new dictionary..."  << '\n'; ;
			if (cmp == __null )       cmp = &default_compare;
			if (displayer == __null ) displayer = &default_displayer;
			if (str_f == __null )     str_f = &default_stringer;
			compare_f = cmp;
			display_f = displayer;
			destroy_f = destroyer;
			string_f  = str_f;
			di = make_dictionary(compare_f);
			append_error(di == __null , "dict internal");
			if (di == __null ) {
				dict_perror("build dictionary");
				exit(errno);
			}
			count = 1;
		}
		~internal()
		{
			if (799  >=  800 ) cout <<  "dict ~internal executing"  << '\n'; ;
			int err = clear_dictionary(di, destroy_f, (&__iob[2]) );
			append_error(err, "clear dictionary");
			if (err) dict_perror("dict ~internal");
			free((void *)di);
		}
	};// ERROR - candidate list for bad call
	internal *rep;
protected:
	int & errno()         { return( rep->errno );     }
	COMPARE compare_f()   { return( rep->compare_f ); }
	DISPLAYER display_f() { return( rep->display_f ); }
	DESTROYER destroy_f() { return( rep->destroy_f ); }
	STRINGER string_f()   { return( rep->string_f  ); }
	void append_error(int expr, const char *proc)
	{
		rep->append_error(expr, proc);
	}
	void obj_error(int expr, const char *proc, const void *obj)
	{
		rep->obj_error(expr, proc, obj);
	}
public:
	static int err;
	static int val;
	static void *p;
	dict()
	{
		if (799  >=  800 ) cout <<  "Default constructor executing..."  << '\n'; ;
		rep = new internal( &default_compare,   &null_destroy,
				    &default_displayer, &default_stringer);// ERROR - no matching fn call
	}
	dict(COMPARE cmp,
	     DESTROYER destroyer = __null ,
	     DISPLAYER displayer = &default_displayer,
	     STRINGER str_f = &default_stringer)
	{// ERROR - invalid type for default argument -- not expecting const
		rep = new internal(cmp, destroyer, displayer, str_f);
	}
	dict(const dict &di2)
	{
		rep = di2.rep;
		rep->count ++;
		if (799  >=  800 ) cout <<  "Incrementing count to " << rep->count  << '\n'; ;
	}
	dict & operator = (const dict &di2)
	{
		di2.rep->count ++;   
		if (799  >=  800 ) cout <<  "Incrementing di2 count to " << di2.rep->count  << '\n'; ;
		if (799  >=  800 ) cout <<  "Decrementing lvalue count to " << rep->count-1  << '\n'; ;
		if (--rep->count <= 0) {
			delete rep;
		}
		rep = di2.rep;
		return( *this );
	}
	~dict()
	{
		if (799  >=  800 ) cout <<  "~dict executing..."  << '\n'; ;
		if (799  >=  800 ) cout <<  "Decrementing dict::internal count to " << rep->count-1  << '\n'; ;
		if (--rep->count <= 0) delete rep;
	}
	int insert(POINTER item)
	{
		err = dict_insert(item, rep->di);
		obj_error(err, "insert", item);
		return errno();
	}
	int remove(POINTER item)
	{
		err = dict_delete(item, rep->di);
		obj_error(err, "remove", item);
		return errno();
	}
	POINTER access(POINTER item)
	{
		p = dict_access(item, rep->di);
		obj_error(p == __null , "access", item);
		return(p);
	}
	POINTER low()
	{
		p = dict_low(rep->di);
		append_error(p == __null , "low");
		return(p);
	}
	POINTER nlow()
	{
		p = dict_nlow(rep->di);
		append_error(p == __null , "nlow");
		return(p);
	}
	POINTER big()
	{
		p = dict_big(rep->di);
		append_error(p == __null , "big");
		return(p);
	}
	POINTER nbig()
	{
		p = dict_nbig(rep->di);
		append_error(p == __null , "nbig");
		return(p);
	}
	POINTER first()
	{
		p = dict_first(rep->di);
		append_error(p == __null , "first");
		return(p);
	}
	POINTER last()
	{
		p = dict_last(rep->di);
		append_error(p == __null , "last");
		return(p);
	}
	POINTER next()
	{
		p = dict_next(rep->di);
		append_error(p == __null , "next");
		return(p);
	}
	POINTER prev()
	{
		p = dict_prev(rep->di);
		append_error(p == __null , "prev");
		return(p);
	}
	int clear(DESTROYER destroy, FILE *fp = (&__iob[2]) )
	{
		err = clear_dictionary(rep->di, destroy, fp);
		append_error(err, "clear");
		return errno();
	}
	int clear(FILE *fp = (&__iob[2]) )
	{
		err = clear_dictionary(rep->di, rep->destroy_f, fp);
		append_error(err, "clear");
		return errno();
	}
	int size()
	{
		val = dict_size(rep->di);
		append_error(val < 0, "size");
		return(val);
	}
	POINTER elem(int index)
	{
		p = dict_elem(index, rep->di);
		append_error(p == __null , "elem");
		return(p);
	}
	int compare(dict &di2)
	{
		val = dict_compare(rep->di, di2.rep->di);
		return(val);
	}
	int height()
	{
		val = dict_height(rep->di);
		append_error(val < 0, "height");
		return(val);
	}
	const char * version()
	{
		return( dict_version() );
	}
	void display(FILE *fp = (&__iob[1]) )
	{
		dict_print_stats(fp, rep->di);
	}
	void print(FILE *fp = (&__iob[1]) , traversal_t tr = INORDER)
	{
		fprintf(fp, "====== DICTIONARY (count=%d) ======\n",
			rep->count);
		dict_print(fp, rep->di, tr, rep->display_f);
	}
	void print(traversal_t tr, FILE *fp = (&__iob[1]) )
	{
		fprintf(fp, "====== DICTIONARY (count=%d) ======\n",
			rep->count);
		dict_print(fp, rep->di, tr, rep->display_f);
	}
	void error(const char *s = "class dict")
	{
		dict_errno = errno();
		dict_perror(s);
		errno() = 0;
	}
	void error_append(const char *s = "class dict")
	{
		dict_append_perror(s);
	}
	const char * errstring()
	{
		return( dict_errstring() );
	}
};
ostream & operator << (ostream & fp, traversal_t tr);
class queue : public object {
	class internal {
	public:
		QUEUE Q;
		QUEUE Q2;
		int errno;
		DISPLAYER display_f;
		DESTROYER destroy_f;
		STRINGER string_f;
		int count;
	        int swap()
		{
			QUEUE tmp = Q;
			Q = Q2;
			Q2 = tmp;
		}
		void append_error(int expr, const char *proc)
		{
			if (expr) queue_append_perror(proc);
			errno = queue_errno;
		}
		void obj_error(int expr, const char *proc, const void *obj)
		{
			if (expr) {
				queue_append_perror((*string_f)(obj));
				queue_append_perror(proc);
			}
			errno = queue_errno;
		}
		internal(DESTROYER destroyer,
		         DISPLAYER displayer, STRINGER str_f)
		{// ERROR - candidate for bad call
			if (799  >=  800 ) cout <<  "Creating new queue..."  << '\n'; ;
			if (displayer == __null ) displayer = &default_displayer;
			if (str_f == __null )     str_f = &default_stringer;
			display_f = displayer;
			destroy_f = destroyer;
			string_f  = str_f;
			Q = make_queue();
			append_error(Q == __null , "queue internal");
			errno = queue_errno;
			if (Q == __null ) {
				queue_perror("build queue");
				exit(errno);
			}
			Q2 = make_queue();
			append_error(Q2 == __null , "queue internal");
			if (Q2 == __null ) {
				queue_perror("build second queue");
				exit(errno);
			}
			count = 1;
		}
		~internal()
		{
			if (799  >=  800 ) cout <<  "queue ~internal executing"  << '\n'; ;
			while ((p = queue_remove(Q)) != __null ) {
				if (destroy_f != __null ) {
					(*destroy_f)((POINTER)&p, (&__iob[2]) );
				}
			}
			queue_errno = 0;
			queue_destroy((POINTER)&Q, (&__iob[2]) );
			if (queue_errno) {
				append_error(queue_errno,
					     "queue_destroy(Q)");
				queue_perror("queue ~internal");
			}
			queue_errno = 0;
			queue_destroy((POINTER)&Q2, (&__iob[2]) );
			if (queue_errno) {
				append_error(queue_errno,
					     "queue_destroy(Q2)");
				queue_perror("queue ~internal");
			}
		}
	};// ERROR - list of candidates for bad call
	internal *rep;
protected:
	int & errno()         { return( rep->errno );     }
	DISPLAYER display_f() { return( rep->display_f ); }
	DESTROYER destroy_f() { return( rep->destroy_f ); }
	STRINGER string_f()   { return( rep->string_f  ); }
	void append_error(int expr, const char *proc)
	{
		rep->append_error(expr, proc);
	}
	void obj_error(int expr, const char *proc, const void *obj)
	{
		rep->obj_error(expr, proc, obj);
	}
public:
	static int err;
	static int val;
	static void *p;
	queue()
	{
		if (799  >=  800 ) cout <<  "Default constructor executing..."  << '\n'; ;
		rep = new internal( &null_destroy,
				    &default_displayer, &default_stringer);// ERROR - no matching fn for call
	}
	queue(DESTROYER destroyer = __null ,
	      DISPLAYER displayer = &default_displayer,
	      STRINGER str_f = &default_stringer)
	{// ERROR - invalid type for default argument -- not expecting const 
		rep = new internal(destroyer, displayer, str_f);
	}
	queue(const queue &Q2)
	{
		rep = Q2.rep;
		rep->count ++;
		if (799  >=  800 ) cout <<  "Incrementing count to " << rep->count  << '\n'; ;
	}
	queue & operator = (const queue &Q2)
	{
		Q2.rep->count ++;   
		if (799  >=  800 ) cout <<  "Incrementing Q2 count to " << Q2.rep->count  << '\n'; ;
		if (799  >=  800 ) cout <<  "Decrementing lvalue count to " << rep->count-1  << '\n'; ;
		if (--rep->count <= 0) {
			delete rep;
		}
		rep = Q2.rep;
		return( *this );
	}
	~queue()
	{
		if (799  >=  800 ) cout <<  "~queue executing..."  << '\n'; ;
		if (799  >=  800 ) cout <<  "Decrementing queue::internal count to " << rep->count-1  << '\n'; ;
		if (--rep->count <= 0) delete rep;
	}
	void error(const char *s = "class queue")
	{
		queue_errno = errno();
		queue_perror(s);
		errno() = 0;
	}
	void error_append(const char *s = "class queue")
	{
		queue_append_perror(s);
	}
	const char * errstring()
	{
		return( queue_errstring() );
	}
	int append(POINTER item)
	{
		err = queue_append(rep->Q, (POINTER)item);
		obj_error(err, "append", item);
		return errno();
	}
	POINTER remove()
	{
		p = queue_remove(rep->Q);
		append_error(p == __null , "remove");
		return(p);
	}
	POINTER look(POINTER item)
	{
		p = queue_look(rep->Q);
		append_error(p == __null , "look");
		return(p);
	}
	POINTER last(POINTER item)
	{
		p = queue_last(rep->Q);
		append_error(p == __null , "last");
		return(p);
	}
	int size()
	{
		val = queue_size(rep->Q);
		append_error(val < 0, "size");
		return(val);
	}
	const char * version()
	{
		return( queue_version() );
	}
	void display(FILE *fp = (&__iob[1]) )
	{
		int i = 1;
		POINTER ptr;
		fprintf(fp, "====== QUEUE DISPLAY (size %d) ======\n",
			size());
		while (ptr = remove()) {
			err = queue_append(rep->Q2, ptr);
			if (err) {
				obj_error(err, "append", ptr);
				error("queue_append(rep->Q2)");
			}
			fprintf(fp, "[%d] ", i++);
			(*rep->display_f)(ptr, fp, (&__iob[2]) );
		}
		rep->swap();
	}
};
long object::current_id = 0;
long object::next_id()
{
	return( ++current_id );
}
void object::display2(ostream & fp) const
{
	fp << "object #" << id;
}
ostream & operator << (ostream & fp, object & obj)
{
	obj.display(fp);
	return(fp);
}
int object::compare(const void *p2) const
{
	const object & z2 = *(const object *) p2;
	if (799  >=  1000 ) cout <<  "*** object::compare " << *this << " with " << z2  << '\n'; ;
	return( id - z2.id );
}
int operator < (object & obj1, object & obj2)
{
	if (obj1.compare( &obj2 ) < 0) return(1);
	else return(0);
}
int operator <= (object & obj1, object & obj2)
{
	if (obj1.compare( &obj2 ) <= 0) return(1);
	else return(0);
}
int operator > (object & obj1, object & obj2)
{
	if (obj1.compare( &obj2 ) > 0) return(1);
	else return(0);
}
int operator >= (object & obj1, object & obj2)
{
	if (obj1.compare( &obj2 ) >= 0) return(1);
	else return(0);
}
int operator == (object & obj1, object & obj2)
{
	if (obj1.compare( &obj2 ) == 0) return(1);
	else return(0);
}
int operator != (object & obj1, object & obj2)
{
	if (obj1.compare( &obj2 ) != 0) return(1);
	else return(0);
}
void null_destroy(void *p, FILE *errorFile)
{
}
int object_compare(const void *p1, const void *p2)
{
	const object *obj1 = (const object *) p1;
	if (799  >=  1000 ) cout <<   "*** Comparing " << *obj1 << " to address " << long(p2)  << '\n'; ;
	return( obj1->compare(p2) );
}
void object_display(const void *p, FILE *fp, FILE *errorFile)
{
	cout << *(object *)p << '\n';
}
void object_destroy(void *p, FILE *errorFile)
{
	object **pObj = (object **)p;
	object *obj = *pObj;
	if (obj == __null ) {
		if (799  >=  10 ) cout <<   "+ object_destroy: NIL object passed in"  << '\n'; ;
		return;
	}
	if (799  >=  998 ) cout <<   "+ object_destroy: delete obj:"  << '\n'; ;
	if (799  >=  998 ) cout <<   *obj  << '\n'; ;
	if (799  >=  998 ) cout <<   "===================================================="  << '\n'; ;
	delete obj;
	*pObj = __null ;
}
int default_compare(const void *p1, const void *p2)
{
	long L1 = long(p1);
	long L2 = long(p2);
	if (799  >=  1000 ) cout <<  "+ default_compare " << long(p1) << ',' << long(p2)  << '\n'; ;
	if (L1 < L2) return(-1);
	else if (L1 > L2) return(1);
	else return(0);
}
const char * default_stringer(const void *ptr)
{
	static char buf[100];
	sprintf(buf, "ADDRESS %lu", (unsigned long)ptr);
	return(buf);
}
void default_displayer(const void *ptr, FILE *fp, FILE *errorFile)
{
	fprintf(fp, "%s\n", default_stringer(ptr));
}
object::operator char *() const
{
	const int max = 10;
	const int maxChar = 16;
	static char buf[max][maxChar];
	static int index = 0;
	char *s = buf[index++ % max];
	sprintf(s, "object #%d", id);
	return(s);
}
const char * object_stringer(const void *ptr)
{
	object & obj = *(object *)ptr;
	return((char *)obj);
}
int dict::err;
int dict::val;
POINTER dict::p;
ostream & operator << (ostream & fp, traversal_t tr)
{
	fp << traversal_string(tr);
	return(fp);
}
int queue::err;
int queue::val;
POINTER queue::p;
