// Special g++ Options: -fcheck-memory-usage
// Build don't link: 

 

























#pragma interface

#define _IOSTREAM_H

 
























#define _STREAMBUF_H

#pragma interface


   

#define _IO_NEW_STREAMS 

extern "C" {
 


























#define _IO_STDIO_H

  

#define _G_config_h
#define _G_LIB_VERSION "0.67"
#define _G_NAMES_HAVE_UNDERSCORE 1
#define _G_VTABLE_LABEL_HAS_LENGTH 1
#define _G_VTABLE_LABEL_PREFIX "__vt$"
#define _G_HAVE_ST_BLKSIZE 1
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



typedef unsigned int _G_size_t;
typedef long _G_time_t;
typedef unsigned short _G_uid_t;



typedef __wchar_t _G_wchar_t;
typedef int   _G_ssize_t;
typedef int   _G_wint_t;
typedef char * _G_va_list;
#define _G_signal_return_type void
#define _G_sprintf_return_type char*

typedef signed char _G_int8_t;

typedef unsigned char _G_uint8_t;
typedef short _G_int16_t;
typedef unsigned short _G_uint16_t;
typedef long _G_int32_t;
typedef unsigned long _G_uint32_t;

#define HAVE_INT64
typedef long long _G_int64_t;
typedef unsigned long long _G_uint64_t;

#define _G_BUFSIZ 1024
#define _G_FOPEN_MAX 32 
#define _G_FILENAME_MAX 1024 
#define _G_NULL 0 

#define _G_ARGS(ARGLIST) ARGLIST









#define _G_HAVE_ATEXIT 0
#define _G_HAVE_SYS_RESOURCE 1
#define _G_HAVE_SYS_SOCKET 1
#define _G_HAVE_SYS_WAIT 1
#define _G_HAVE_UNISTD 1
#define _G_HAVE_DIRENT 1
#define _G_HAVE_CURSES 1
#define _G_MATH_H_INLINES 0
#define _G_HAVE_BOOL 1


#define _IO_pos_t _G_fpos_t 
#define _IO_fpos_t _G_fpos_t
#define _IO_size_t _G_size_t
#define _IO_ssize_t _G_ssize_t
#define _IO_off_t _G_off_t
#define _IO_pid_t _G_pid_t
#define _IO_uid_t _G_uid_t
#define _IO_HAVE_SYS_WAIT _G_HAVE_SYS_WAIT
#define _IO_HAVE_ST_BLKSIZE _G_HAVE_ST_BLKSIZE
#define _IO_BUFSIZ _G_BUFSIZ
#define _IO_va_list _G_va_list








#define __P(protos) protos






 

#define _PARAMS(protos) __P(protos)





#define _IO_UNIFIED_JUMPTABLES 1

#define _IO_USE_DTOA 1



#define EOF (-1)




#define NULL (__null)









#define _IOS_INPUT	1
#define _IOS_OUTPUT	2
#define _IOS_ATEND	4
#define _IOS_APPEND	8
#define _IOS_TRUNC	16
#define _IOS_NOCREATE	32
#define _IOS_NOREPLACE	64
#define _IOS_BIN	128

 





#define _IO_MAGIC 0xFBAD0000 
#define _OLD_STDIO_MAGIC 0xFABC0000 
#define _IO_MAGIC_MASK 0xFFFF0000
#define _IO_USER_BUF 1 
#define _IO_UNBUFFERED 2
#define _IO_NO_READS 4 
#define _IO_NO_WRITES 8 
#define _IO_EOF_SEEN 0x10
#define _IO_ERR_SEEN 0x20
#define _IO_DELETE_DONT_CLOSE 0x40 
#define _IO_LINKED 0x80 
#define _IO_IN_BACKUP 0x100
#define _IO_LINE_BUF 0x200
#define _IO_TIED_PUT_GET 0x400 
#define _IO_CURRENTLY_PUTTING 0x800
#define _IO_IS_APPENDING 0x1000
#define _IO_IS_FILEBUF 0x2000
#define _IO_BAD_SEEN 0x4000

 
#define _IO_SKIPWS 01
#define _IO_LEFT 02
#define _IO_RIGHT 04
#define _IO_INTERNAL 010
#define _IO_DEC 020
#define _IO_OCT 040
#define _IO_HEX 0100
#define _IO_SHOWBASE 0200
#define _IO_SHOWPOINT 0400
#define _IO_UPPERCASE 01000
#define _IO_SHOWPOS 02000
#define _IO_SCIENTIFIC 04000
#define _IO_FIXED 010000
#define _IO_UNITBUF 020000
#define _IO_STDIO 040000
#define _IO_DONT_CLOSE 0100000
#define _IO_BOOLALPHA 0200000


struct _IO_jump_t;  struct _IO_FILE;

 









    typedef void _IO_lock_t;





 

struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;
   

   
  int _pos;

};

struct _IO_FILE {
  int _flags;		 
#define _IO_file_flags _flags

   
   
  char* _IO_read_ptr;	 
  char* _IO_read_end;	 
  char* _IO_read_base;	 
  char* _IO_write_base;	 
  char* _IO_write_ptr;	 
  char* _IO_write_end;	 
  char* _IO_buf_base;	 
  char* _IO_buf_end;	 
   
  char *_IO_save_base;  
  char *_IO_backup_base;   
  char *_IO_save_end;  

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _blksize;
  _G_off_t  _offset;

#define __HAVE_COLUMN 
   
  unsigned short _cur_column;
  char _unused;
  char _shortbuf[1];

   




};





struct _IO_FILE_plus;
extern struct _IO_FILE_plus _IO_stdin_, _IO_stdout_, _IO_stderr_;
#define _IO_stdin ((_IO_FILE*)(&_IO_stdin_))
#define _IO_stdout ((_IO_FILE*)(&_IO_stdout_))
#define _IO_stderr ((_IO_FILE*)(&_IO_stderr_))


 
typedef struct
{
  _G_ssize_t  (*read)  (struct _IO_FILE *, void *, _G_ssize_t )  ;
  _G_ssize_t  (*write)  (struct _IO_FILE *, const void *, _G_ssize_t )  ;
  _G_fpos_t  (*seek)  (struct _IO_FILE *, _G_off_t , int)  ;
  int (*close)  (struct _IO_FILE *)  ;
} _IO_cookie_io_functions_t;

 
struct _IO_cookie_file
{
  struct _IO_FILE file;
  const void *vtable;
  void *cookie;
  _IO_cookie_io_functions_t io_functions;
};



extern "C" {


extern int __underflow  (_IO_FILE *)  ;
extern int __uflow  (_IO_FILE *)  ;
extern int __overflow  (_IO_FILE *, int)  ;

#define _IO_getc_unlocked(_fp) ((_fp)->_IO_read_ptr >= (_fp)->_IO_read_end ? __uflow (_fp) : *(unsigned char *) (_fp)->_IO_read_ptr++)


#define _IO_peekc_unlocked(_fp) ((_fp)->_IO_read_ptr >= (_fp)->_IO_read_end && __underflow (_fp) == EOF ? EOF : *(unsigned char *) (_fp)->_IO_read_ptr)




#define _IO_putc_unlocked(_ch, _fp) (((_fp)->_IO_write_ptr >= (_fp)->_IO_write_end) ? __overflow (_fp, (unsigned char) (_ch)) : (unsigned char) (*(_fp)->_IO_write_ptr++ = (_ch)))




#define _IO_feof_unlocked(__fp) (((__fp)->_flags & _IO_EOF_SEEN) != 0)
#define _IO_ferror_unlocked(__fp) (((__fp)->_flags & _IO_ERR_SEEN) != 0)

extern int _IO_getc  (_IO_FILE *__fp)  ;
extern int _IO_putc  (int __c, _IO_FILE *__fp)  ;
extern int _IO_feof  (_IO_FILE *__fp)  ;
extern int _IO_ferror  (_IO_FILE *__fp)  ;

extern int _IO_peekc_locked  (_IO_FILE *__fp)  ;

 
#define _IO_PENDING_OUTPUT_COUNT(_fp)	((_fp)->_IO_write_ptr - (_fp)->_IO_write_base)


extern void _IO_flockfile  (_IO_FILE *)  ;
extern void _IO_funlockfile  (_IO_FILE *)  ;
extern int _IO_ftrylockfile  (_IO_FILE *)  ;




#define _IO_peekc(_fp) _IO_peekc_unlocked (_fp)
#define _IO_flockfile(_fp) 
#define _IO_funlockfile(_fp) 
#define _IO_ftrylockfile(_fp) 
#define _IO_cleanup_region_start(_fct, _fp) 
#define _IO_cleanup_region_end(_Doit) 



extern int _IO_vfscanf  (_IO_FILE *, const char *, _G_va_list , int *)  ;
extern int _IO_vfprintf  (_IO_FILE *, const char *, _G_va_list )  ;
extern _G_ssize_t  _IO_padn  (_IO_FILE *, int, _G_ssize_t )  ;
extern _G_size_t  _IO_sgetn  (_IO_FILE *, void *, _G_size_t )  ;

extern _G_fpos_t  _IO_seekoff  (_IO_FILE *, _G_off_t , int, int)  ;
extern _G_fpos_t  _IO_seekpos  (_IO_FILE *, _G_fpos_t , int)  ;

extern void _IO_free_backup_area  (_IO_FILE *)  ;


}




}
 



















#define _IO_wchar_t short


extern "C++" {
class istream;  
class ostream; class streambuf;

 



typedef _G_off_t  streamoff;
typedef _G_fpos_t  streampos;
typedef _G_ssize_t  streamsize;

typedef unsigned long __fmtflags;
typedef unsigned char __iostate;

struct _ios_fields
{  
    streambuf *_strbuf;
    ostream* _tie;
    int _width;
    __fmtflags _flags;
    short  _fill;
    __iostate _state;
    __iostate _exceptions;
    int _precision;

    void *_arrays;  
};

#define _IOS_GOOD	0
#define _IOS_EOF	1
#define _IOS_FAIL	2
#define _IOS_BAD	4

#define _IO_INPUT	1
#define _IO_OUTPUT	2
#define _IO_ATEND	4
#define _IO_APPEND	8
#define _IO_TRUNC	16
#define _IO_NOCREATE	32
#define _IO_NOREPLACE	64
#define _IO_BIN		128



class ios : public _ios_fields {
  ios& operator=(ios&);   
  ios (const ios&);  
  public:
    typedef __fmtflags fmtflags;
    typedef int iostate;
    typedef int openmode;
    typedef int streamsize;
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
	bin = 128 ,  
	binary = 128  };
    enum seek_dir { beg, cur, end};
    typedef enum seek_dir seekdir;
     
    enum { skipws= 01 ,
	   left= 02 , right= 04 , internal= 010 ,
	   dec= 020 , oct= 040 , hex= 0100 ,
	   showbase= 0200 , showpoint= 0400 ,
	   uppercase= 01000 , showpos= 02000 ,
	   scientific= 04000 , fixed= 010000 ,
	   unitbuf= 020000 , stdio= 040000 



	   };
    enum {  
	basefield=dec+oct+hex,
	floatfield = scientific+fixed,
	adjustfield = left+right+internal
    };



    ostream* tie() const { return _tie; }
    ostream* tie(ostream* val) { ostream* save=_tie; _tie=val; return save; }

     
    short  fill() const { return (short )_fill; }
    short  fill(short  newf)
	{short  oldf = (short )_fill; _fill = (char)newf; return oldf;}
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
	fmtflags oldbits = _flags;
	_flags &= ~mask; return oldbits; }
    int width() const { return _width; }
    int width(int val) { int save = _width; _width = val; return save; }




    void _throw_failure() const { }

    void clear(iostate state = 0) {
	_state = _strbuf ? state : state|badbit;
	if (_state & _exceptions) _throw_failure(); }
    void set(iostate flag) { _state |= flag;
	if (_state & _exceptions) _throw_failure(); }
    void setstate(iostate flag) { _state |= flag;  
	if (_state & _exceptions) _throw_failure(); }
    int good() const { return _state == 0; }
    int eof() const { return _state & ios::eofbit; }
    int fail() const { return _state & (ios::badbit|ios::failbit); }
    int bad() const { return _state & ios::badbit; }
    iostate rdstate() const { return _state; }
    operator void*() const { return fail() ? (void*)0 : (void*)(-1); }
    int operator!() const { return fail(); }
    iostate exceptions() const { return _exceptions; }
    void exceptions(iostate enable) {
	_exceptions = enable;
	if (_state & _exceptions) _throw_failure(); }

    streambuf* rdbuf() const { return _strbuf; }
    streambuf* rdbuf(streambuf *_s) {
      streambuf *_old = _strbuf; _strbuf = _s; clear (); return _old; }

    static int sync_with_stdio(int on);
    static void sync_with_stdio() { sync_with_stdio(1); }
    static fmtflags bitalloc();
    static int xalloc();
    void*& pword(int);
    void* pword(int) const;
    long& iword(int);
    long iword(int) const;









     
    class Init {
    public:
      Init () { }
    };

  protected:
    inline ios(streambuf* sb = 0, ostream* tie_to = 0);
    inline virtual ~ios();
    inline void init(streambuf* sb, ostream* tie = 0);
};




typedef ios::seek_dir _seek_dir;


 
 
 
 
 

 
 
class streammarker : private _IO_marker {
    friend class streambuf;
    void set_offset(int offset) { _pos = offset; }
  public:
    streammarker(streambuf *sb);
    ~streammarker();
    int saving() { return  1; }
    int delta(streammarker&);
    int delta();
};

struct streambuf : public _IO_FILE {  
    friend class ios;
    friend class istream;
    friend class ostream;
    friend class streammarker;
    const void *&_vtable() { return *(const void**)((_IO_FILE*)this + 1); }
  protected:
    static streambuf* _list_all;  
    _IO_FILE*& xchain() { return _chain; }
    void _un_link();
    void _link_in();
    char* gptr() const
      { return _flags  & 0x100  ? _IO_save_base : _IO_read_ptr; }
    char* pptr() const { return _IO_write_ptr; }
    char* egptr() const
      { return _flags  & 0x100  ? _IO_save_end : _IO_read_end; }
    char* epptr() const { return _IO_write_end; }
    char* pbase() const { return _IO_write_base; }
    char* eback() const
      { return _flags  & 0x100  ? _IO_save_base : _IO_read_base;}
    char* base() const { return _IO_buf_base; }
    char* ebuf() const { return _IO_buf_end; }
    int blen() const { return _IO_buf_end - _IO_buf_base; }
    void xput_char(char c) { *_IO_write_ptr++ = c; }
    int xflags() { return _flags ; }
    int xflags(int f) {int fl = _flags ; _flags  = f; return fl;}
    void xsetflags(int f) { _flags  |= f; }
    void xsetflags(int f, int mask)
      { _flags  = (_flags  & ~mask) | (f & mask); }
    void gbump(int n)
      { _flags  & 0x100  ? (_IO_save_base+=n):(_IO_read_ptr+=n);}
    void pbump(int n) { _IO_write_ptr += n; }
    void setb(char* b, char* eb, int a=0);
    void setp(char* p, char* ep)
      { _IO_write_base=_IO_write_ptr=p; _IO_write_end=ep; }
    void setg(char* eb, char* g, char *eg) {
      if (_flags  & 0x100 ) _IO_free_backup_area(this); 
      _IO_read_base = eb; _IO_read_ptr = g; _IO_read_end = eg; }
    char *shortbuf() { return _shortbuf; }

    int in_backup() { return _flags & 0x100 ; }
     
    char *Gbase() { return in_backup() ? _IO_save_base : _IO_read_base; }
     
    char *eGptr() { return in_backup() ? _IO_save_end : _IO_read_end; }
     
    char *Bbase() { return in_backup() ? _IO_read_base : _IO_save_base; }
    char *Bptr() { return _IO_backup_base; }
     
    char *eBptr() { return in_backup() ? _IO_read_end : _IO_save_end; }
    char *Nbase() { return _IO_save_base; }
    char *eNptr() { return _IO_save_end; }
    int have_backup() { return _IO_save_base != (__null) ; }
    int have_markers() { return _markers != (__null) ; }
    void free_backup_area();
    void unsave_markers();  
    int put_mode() { return _flags & 0x800 ; }
    int switch_to_get_mode();
    
    streambuf(int flags=0);
  public:
    static int flush_all();
    static void flush_all_linebuffered();  
    virtual ~streambuf();
    virtual int overflow(int c = (-1) );  
    virtual int underflow();  
    virtual int uflow();  
    virtual int pbackfail(int c);
 
    virtual streamsize xsputn(const char* s, streamsize n);
    virtual streamsize xsgetn(char* s, streamsize n);
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streampos seekpos(streampos pos, int mode = ios::in|ios::out);

    streampos pubseekoff(streamoff o, _seek_dir d, int mode=ios::in|ios::out)
      { return _IO_seekoff (this, o, d, mode); }
    streampos pubseekpos(streampos pos, int mode = ios::in|ios::out)
      { return _IO_seekpos (this, pos, mode); }
    streampos sseekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    streampos sseekpos(streampos pos, int mode = ios::in|ios::out);
    virtual streambuf* setbuf(char* p, int len);
    virtual int sync();
    virtual int doallocate();

    int seekmark(streammarker& mark, int delta = 0);
    int sputbackc(char c);
    int sungetc();
    int unbuffered() { return _flags & 2  ? 1 : 0; }
    int linebuffered() { return _flags & 0x200  ? 1 : 0; }
    void unbuffered(int i)
	{ if (i) _flags |= 2 ; else _flags &= ~2 ; }
    void linebuffered(int i)
	{ if (i) _flags |= 0x200 ; else _flags &= ~0x200 ; }
    int allocate() {  
	if (base() || unbuffered()) return 0;
	else return doallocate(); }
     
    void allocbuf() { if (base() == (__null) ) doallocbuf(); }
    void doallocbuf();
    int in_avail() { return _IO_read_end - _IO_read_ptr; }
    int out_waiting() { return _IO_write_ptr - _IO_write_base; }
    streamsize sputn(const char* s, streamsize n) { return xsputn(s, n); }
    streamsize padn(char pad, streamsize n) { return _IO_padn(this, pad, n); }
    streamsize sgetn(char* s, streamsize n) { return _IO_sgetn(this, s, n); }
    int ignore(int);
    int get_column();
    int set_column(int);
    long sgetline(char* buf, _G_size_t  n, char delim, int putback_delim);
    int sputc(int c) { return _IO_putc(c, this); }
    int sbumpc() { return _IO_getc(this); }
    int sgetc() { return ((  this  )->_IO_read_ptr >= (  this  )->_IO_read_end && __underflow (  this  ) == (-1)  ? (-1)  : *(unsigned char *) (  this  )->_IO_read_ptr)  ; }
    int snextc() {
	if (_IO_read_ptr >= _IO_read_end && __underflow(this) == (-1) )
	  return (-1) ;
	else return _IO_read_ptr++, sgetc(); }
    void stossc() { if (_IO_read_ptr < _IO_read_end) _IO_read_ptr++; }
    int vscan(char const *fmt0, _G_va_list  ap, ios* stream = (__null) );
    int scan(char const *fmt0 ...);
    int vform(char const *fmt0, _G_va_list  ap);
    int form(char const *fmt0 ...);




    virtual streamsize sys_read(char* buf, streamsize size);
    virtual streamsize sys_write(const char*, streamsize);
    virtual streampos sys_seek(streamoff, _seek_dir);
    virtual int sys_close();
    virtual int sys_stat(void*);  
};

 
 

class filebuf : public streambuf {
  protected:
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
    int is_open() const { return _fileno >= 0; }
    int fd() const { return is_open() ? _fileno : (-1) ; }
    filebuf* close();
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streambuf* setbuf(char* p, int len);
    streamsize xsputn(const char* s, streamsize n);
    streamsize xsgetn(char* s, streamsize n);
    virtual int sync();
  protected:  
 
    int is_reading() { return eback() != egptr(); }
    char* cur_ptr() { return is_reading() ?  gptr() : pptr(); }
     
    char* file_ptr() { return eGptr(); }
     
    virtual streamsize sys_read(char* buf, streamsize size);
    virtual streampos sys_seek(streamoff, _seek_dir);
    virtual streamsize sys_write(const char*, streamsize);
    virtual int sys_stat(void*);  
    virtual int sys_close();




};

inline void ios::init(streambuf* sb, ostream* tie_to) {
		_state = sb ? ios::goodbit : ios::badbit; _exceptions=0;
		_strbuf=sb; _tie = tie_to; _width=0; _fill=' ';

		_flags=ios::skipws|ios::dec;



		_precision=6; _arrays = 0; }

inline ios::ios(streambuf* sb, ostream* tie_to) { init(sb, tie_to); }

inline ios::~ios() {



    operator delete(_arrays);
}
}  



extern "C++" {
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
    ostream(streambuf* sb, ostream* tied= (__null) );
    int opfx() {
	if (!good()) return 0;
	else { if (_tie) _tie->flush();  ; return 1;} }
    void osfx() {  ;
		  if (flags() & (ios::unitbuf|ios::stdio))
		      do_osfx(); }
    ostream& flush();
    ostream& put(char c) { _strbuf->sputc(c); return *this; }





    ostream& write(const char *s, streamsize n);
    ostream& write(const unsigned char *s, streamsize n)
      { return write((const char*)s, n);}
    ostream& write(const signed char *s, streamsize n)
      { return write((const char*)s, n);}
    ostream& write(const void *s, streamsize n)
      { return write((const char*)s, n);}
    ostream& seekp(streampos);
    ostream& seekp(streamoff, _seek_dir);
    streampos tellp();
    ostream& form(const char *format ...);
    ostream& vform(const char *format, _G_va_list  args);

    ostream& operator<<(char c);
    ostream& operator<<(unsigned char c) { return (*this) << (char)c; }
    ostream& operator<<(signed char c) { return (*this) << (char)c; }
    ostream& operator<<(const char *s);
    ostream& operator<<(const unsigned char *s)
	{ return (*this) << (const char*)s; }
    ostream& operator<<(const signed char *s)
	{ return (*this) << (const char*)s; }
    ostream& operator<<(const void *p);
    ostream& operator<<(int n);
    ostream& operator<<(unsigned int n);
    ostream& operator<<(long n);
    ostream& operator<<(unsigned long n);

    __extension__ ostream& operator<<(long long n);
    __extension__ ostream& operator<<(unsigned long long n);

    ostream& operator<<(short n) {return operator<<((int)n);}
    ostream& operator<<(unsigned short n) {return operator<<((unsigned int)n);}

    ostream& operator<<(bool b) { return operator<<((int)b); }

    ostream& operator<<(double n);
    ostream& operator<<(float n) { return operator<<((double)n); }



    ostream& operator<<(long double n) { return operator<<((double)n); }

    ostream& operator<<(__omanip func) { return (*func)(*this); }
    ostream& operator<<(__manip func) {(*func)(*this); return *this;}
    ostream& operator<<(streambuf*);



};

class istream : virtual public ios
{
     
protected:
    _G_size_t  _gcount;

    int _skip_ws();
  public:
    istream(): _gcount (0) { }
    istream(streambuf* sb, ostream*tied= (__null) );
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
    istream& read(char *ptr, streamsize n);
    istream& read(unsigned char *ptr, streamsize n)
      { return read((char*)ptr, n); }
    istream& read(signed char *ptr, streamsize n)
      { return read((char*)ptr, n); }
    istream& read(void *ptr, streamsize n)
      { return read((char*)ptr, n); }
    istream& get(streambuf& sb, char delim = '\n');
    istream& gets(char **s, char delim = '\n');
    int ipfx(int need = 0) {
	if (!good()) { set(ios::failbit); return 0; }
	else {
	   ;
	  if (_tie && (need == 0 || rdbuf()->in_avail() < need)) _tie->flush();
	  if (!need && (flags() & ios::skipws)) return _skip_ws();
	  else return 1;
	}
    }
    int ipfx0() {  
	if (!good()) { set(ios::failbit); return 0; }
	else {
	   ;
	  if (_tie) _tie->flush();
	  if (flags() & ios::skipws) return _skip_ws();
	  else return 1;
	}
    }
    int ipfx1() {  
	if (!good()) { set(ios::failbit); return 0; }
	else {
	   ;
	  if (_tie && rdbuf()->in_avail() == 0) _tie->flush();
	  return 1;
	}
    }
    void isfx() {  ; }
    int get() { if (!ipfx1()) return (-1) ;
		else { int ch = _strbuf->sbumpc();
		       if (ch == (-1) ) set(ios::eofbit);
		       return ch;
		     } }
    int peek();
    _G_size_t  gcount() { return _gcount; }
    istream& ignore(int n=1, int delim = (-1) );
    int sync ();
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
    istream& vscan(const char *format, _G_va_list  args);






    istream& operator>>(char*);
    istream& operator>>(unsigned char* p) { return operator>>((char*)p); }
    istream& operator>>(signed char*p) { return operator>>((char*)p); }
    istream& operator>>(char& c);
    istream& operator>>(unsigned char& c) {return operator>>((char&)c);}
    istream& operator>>(signed char& c) {return operator>>((char&)c);}
    istream& operator>>(int&);
    istream& operator>>(long&);

    __extension__ istream& operator>>(long long&);
    __extension__ istream& operator>>(unsigned long long&);

    istream& operator>>(short&);
    istream& operator>>(unsigned int&);
    istream& operator>>(unsigned long&);
    istream& operator>>(unsigned short&);

    istream& operator>>(bool&);

    istream& operator>>(float&);
    istream& operator>>(double&);
    istream& operator>>(long double&);
    istream& operator>>( __manip func) {(*func)(*this); return *this;}
    istream& operator>>(__imanip func) { return (*func)(*this); }
    istream& operator>>(streambuf*);
};

class iostream : public istream, public ostream
{
  public:
    iostream() { }
    iostream(streambuf* sb, ostream*tied= (__null) );
};

class _IO_istream_withassign : public istream {
public:
  _IO_istream_withassign& operator=(istream&);
  _IO_istream_withassign& operator=(_IO_istream_withassign& rhs)
    { return operator= (static_cast<istream&> (rhs)); }
};

class _IO_ostream_withassign : public ostream {
public:
  _IO_ostream_withassign& operator=(ostream&);
  _IO_ostream_withassign& operator=(_IO_ostream_withassign& rhs)
    { return operator= (static_cast<ostream&> (rhs)); }
};

extern _IO_istream_withassign cin;
 
extern _IO_ostream_withassign cout, cerr;

extern _IO_ostream_withassign clog



;

extern istream& lock(istream& ins);
extern istream& unlock(istream& ins);
extern ostream& lock(ostream& outs);
extern ostream& unlock(ostream& outs);

struct Iostream_init { } ;   

inline ios& dec(ios& i)
{ i.setf(ios::dec, ios::dec|ios::hex|ios::oct); return i; }
inline ios& hex(ios& i)
{ i.setf(ios::hex, ios::dec|ios::hex|ios::oct); return i; }
inline ios& oct(ios& i)
{ i.setf(ios::oct, ios::dec|ios::hex|ios::oct); return i; }
}  



 

























#pragma interface

#define _IOMANIP_H



extern "C++" {
 
 
 

 
 
 
 
template<class TP> class smanip;  

template<class TP> class sapp {
    ios& (*_f)(ios&, TP);
public: 
    sapp(ios& (*f)(ios&, TP)) : _f(f) {}
     
    smanip<TP> operator()(TP a) 
      { return smanip<TP>(_f, a); }
};

template<class TP>
inline istream& operator>>(istream& i, const smanip<TP>& m);
template<class TP>
inline ostream& operator<<(ostream& o, const smanip<TP>& m);

template <class TP> class smanip {
    ios& (*_f)(ios&, TP);
    TP _a;
public:
    smanip(ios& (*f)(ios&, TP), TP a) : _f(f), _a(a) {}
     
    friend 
      istream& operator>> <>(istream& i, const smanip<TP>& m);
    friend
      ostream& operator<< <>(ostream& o, const smanip<TP>& m);
};


extern template class smanip<int>;
extern template class smanip<ios::fmtflags>;


template<class TP>
inline istream& operator>>(istream& i, const smanip<TP>& m)
{ (*m._f)(i, m._a); return i; }

template<class TP>
inline ostream& operator<<(ostream& o, const smanip<TP>& m)
{ (*m._f)(o, m._a); return o;}


extern template istream& operator>>(istream&, const smanip<int>&);
extern template istream& operator>>(istream&, const smanip<ios::fmtflags>&);
extern template ostream& operator<<(ostream&, const smanip<int>&);
extern template ostream& operator<<(ostream&, const smanip<ios::fmtflags>&);


 
 
 
 
template<class TP> class imanip; 

template<class TP> class iapp {
    istream& (*_f)(istream&, TP);
public: 
    iapp(istream& (*f)(istream&,TP)) : _f(f) {}
     
    imanip<TP> operator()(TP a)
       { return imanip<TP>(_f, a); }
};

template <class TP>
inline istream& operator>>(istream&, const imanip<TP>&);

template <class TP> class imanip {
    istream& (*_f)(istream&, TP);
    TP _a;
public:
    imanip(istream& (*f)(istream&, TP), TP a) : _f(f), _a(a) {}
     
    friend
      istream& operator>> <>(istream& i, const imanip<TP>& m);
};

template <class TP>
inline istream& operator>>(istream& i, const imanip<TP>& m)
{ return (*m._f)( i, m._a); }

 
 
 
 
template<class TP> class omanip; 

template<class TP> class oapp {
    ostream& (*_f)(ostream&, TP);
public: 
    oapp(ostream& (*f)(ostream&,TP)) : _f(f) {}
     
    omanip<TP> operator()(TP a)
      { return omanip<TP>(_f, a); }
};

template <class TP>
inline ostream& operator<<(ostream&, const omanip<TP>&);

template <class TP> class omanip {
    ostream& (*_f)(ostream&, TP);
    TP _a;
public:
    omanip(ostream& (*f)(ostream&, TP), TP a) : _f(f), _a(a) {}
     
    friend
      ostream& operator<< <>(ostream& o, const omanip<TP>& m);
};

template <class TP>
inline ostream& operator<<(ostream& o, const omanip<TP>& m)
{ return (*m._f)(o, m._a); }

 
 
 

 
 
 
 
#define __DEFINE_IOMANIP_FN1(type,param,function) extern ios& __iomanip_##function (ios&, param); inline type<param> function (param n) { return type<param> (__iomanip_##function, n); }




extern ios& __iomanip_setbase  (ios&,   int ); inline   smanip <  int >   setbase  (  int  n) { return   smanip <  int > (__iomanip_setbase , n); } 
extern ios& __iomanip_setfill  (ios&,   int ); inline   smanip <  int >   setfill  (  int  n) { return   smanip <  int > (__iomanip_setfill , n); } 
extern ios& __iomanip_setprecision  (ios&,   int ); inline   smanip <  int >   setprecision  (  int  n) { return   smanip <  int > (__iomanip_setprecision , n); } 
extern ios& __iomanip_setw  (ios&,   int ); inline   smanip <  int >   setw  (  int  n) { return   smanip <  int > (__iomanip_setw , n); } 

extern ios& __iomanip_resetiosflags  (ios&,   ios::fmtflags ); inline   smanip <  ios::fmtflags >   resetiosflags  (  ios::fmtflags  n) { return   smanip <  ios::fmtflags > (__iomanip_resetiosflags , n); } 
extern ios& __iomanip_setiosflags  (ios&,   ios::fmtflags ); inline   smanip <  ios::fmtflags >   setiosflags  (  ios::fmtflags  n) { return   smanip <  ios::fmtflags > (__iomanip_setiosflags , n); } 
}  




#define LS_hh


#define Mouvement_hh



#define usuel_hh

 




#define 	_MATH_H


extern "C" {


#pragma ident	"@(#)math.h	2.5	95/02/07"















 


typedef union _h_val {
  	unsigned long _i[2];
	double _d;
} _h_val;


extern const _h_val __huge_val;





#define 	HUGE_VAL __huge_val._d




 


#define 	M_E		2.7182818284590452354
#define 	M_LOG2E		1.4426950408889634074
#define 	M_LOG10E	0.43429448190325182765
#define 	M_LN2		0.69314718055994530942
#define 	M_LN10		2.30258509299404568402
#define 	M_PI		3.14159265358979323846
#define 	M_PI_2		1.57079632679489661923
#define 	M_PI_4		0.78539816339744830962
#define 	M_1_PI		0.31830988618379067154
#define 	M_2_PI		0.63661977236758134308
#define 	M_2_SQRTPI	1.12837916709551257390
#define 	M_SQRT2		1.41421356237309504880
#define 	M_SQRT1_2	0.70710678118654752440

extern int signgam;

#define 	MAXFLOAT	((float)3.40282346638528860e+38)


 


enum version {libm_ieee = -1, c_issue_4, ansi_1, strict_ansi};


extern const enum version _lib_version;





#define exception __math_exception

struct __math_exception  {

#undef exception

	int type;
	char *name;
	double arg1;
	double arg2;
	double retval;
};

#define 	HUGE		MAXFLOAT

#define 	_ABS(x)		((x) < 0 ? -(x) : (x))

#define 	_REDUCE(TYPE, X, XN, C1, C2)	{ double x1 = (double)(TYPE)X, x2 = X - x1; X = x1 - (XN) * (C1); X += x2; X -= (XN) * (C2); }



#define 	DOMAIN		1
#define 	SING		2
#define 	OVERFLOW	3
#define 	UNDERFLOW	4
#define 	TLOSS		5
#define 	PLOSS		6

#define 	_POLY1(x, c)	((c)[0] * (x) + (c)[1])
#define 	_POLY2(x, c)	(_POLY1((x), (c)) * (x) + (c)[2])
#define 	_POLY3(x, c)	(_POLY2((x), (c)) * (x) + (c)[3])
#define 	_POLY4(x, c)	(_POLY3((x), (c)) * (x) + (c)[4])
#define 	_POLY5(x, c)	(_POLY4((x), (c)) * (x) + (c)[5])
#define 	_POLY6(x, c)	(_POLY5((x), (c)) * (x) + (c)[6])
#define 	_POLY7(x, c)	(_POLY6((x), (c)) * (x) + (c)[7])
#define 	_POLY8(x, c)	(_POLY7((x), (c)) * (x) + (c)[8])
#define 	_POLY9(x, c)	(_POLY8((x), (c)) * (x) + (c)[9])



 


extern double acos  (double)  ;
extern double asin  (double)  ;
extern double atan  (double)  ;
extern double atan2  (double, double)  ;
extern double cos  (double)  ;
extern double sin  (double)  ;
extern double tan  (double)  ;

extern double cosh  (double)  ;
extern double sinh  (double)  ;
extern double tanh  (double)  ;

extern double exp  (double)  ;
extern double frexp  (double, int *)  ;
extern double ldexp  (double, int)  ;
extern double log  (double)  ;
extern double log10  (double)  ;
extern double modf  (double, double *)  ;

extern double pow  (double, double)  ;
extern double sqrt  (double)  ;

extern double ceil  (double)  ;
extern double fabs  (double)  ;
extern double floor  (double)  ;
extern double fmod  (double, double)  ;



 


extern double erf  (double)  ;
extern double erfc  (double)  ;
extern double gamma  (double)  ;
extern double hypot  (double, double)  ;
extern int isnan  (double)  ;
extern double j0  (double)  ;
extern double j1  (double)  ;
extern double jn  (int, double)  ;
extern double lgamma  (double)  ;
extern double y0  (double)  ;
extern double y1  (double)  ;
extern double yn  (int, double)  ;



 


extern double acosh  (double)  ;
extern double asinh  (double)  ;
extern double atanh  (double)  ;
extern double cbrt  (double)  ;
extern double logb  (double)  ;
extern double nextafter  (double, double)  ;
extern double remainder  (double, double)  ;
extern double scalb  (double, double)  ;

 


extern double expm1  (double)  ;
extern int ilogb  (double)  ;
extern double log1p  (double)  ;
extern double rint  (double)  ;



 



#define exception __math_exception

extern int matherr  (struct __math_exception  *)  ;

#undef exception


 


extern double significand  (double)  ;

 


extern double copysign  (double, double)  ;
extern double scalbn  (double, int)  ;

 








 


extern float modff  (float, float *)  ;

 
 

 
 
 


#define _FLOATINGPOINT_H


extern "C" {


#pragma ident	"@(#)floatingpoint.h	2.4 94/06/09"

 




 










 
 

 
 
 

 




#define 	_STDIO_H

#pragma ident	"@(#)stdio.h	1.39	95/12/04 SMI"	

 
 

 
 
 


#define 	_SYS_FEATURE_TESTS_H

#pragma ident	"@(#)feature_tests.h	1.7	94/12/06 SMI"


extern "C" {


 












}




 





#define 	_SYS_VA_LIST_H

#pragma ident	"@(#)va_list.h	1.6	96/01/26 SMI"

 










extern "C" {





typedef void *__va_list;







}






extern "C" {



#define 	_SIZE_T
typedef unsigned int	size_t;


typedef long	fpos_t;










#define 	BUFSIZ	1024

 










 

















#define 	_NFILE	20	



#define 	_SBFSIZ	8	

#define 	_IOFBF		0000	
#define 	_IOLBF		0100	
#define 	_IONBF		0004	
#define 	_IOEOF		0020	
#define 	_IOERR		0040	

#define 	_IOREAD		0001	
#define 	_IOWRT		0002	
#define 	_IORW		0200	
#define 	_IOMYBUF	0010	





#define 	FOPEN_MAX	_NFILE
#define 	FILENAME_MAX    1024	

#define 	SEEK_SET	0
#define 	SEEK_CUR	1
#define 	SEEK_END	2
#define 	TMP_MAX		17576	




#define 	L_ctermid	9
#define 	L_cuserid	9





#define 	P_tmpdir	"/var/tmp/"


#define 	L_tmpnam	25	


#define 	stdin	(&__iob[0])
#define 	stdout	(&__iob[1])
#define 	stderr	(&__iob[2])






typedef struct	 
{




	int		_cnt;	 
	unsigned char	*_ptr;	 

	unsigned char	*_base;	 
	unsigned char	_flag;	 
	unsigned char	_file;	 
} FILE;


extern FILE		__iob[20 ];



extern FILE		*_lastbuf;
extern unsigned char	*_bufendtab[];

extern unsigned char	 _sibuf[], _sobuf[];




extern int	remove(const char *);
extern int	rename(const char *, const char *);
extern FILE	*tmpfile(void);
extern char	*tmpnam(char *);



extern int	fclose(FILE *);
extern int	fflush(FILE *);
extern FILE	*fopen(const char *, const char *);
extern FILE	*freopen(const char *, const char *, FILE *);
extern void	setbuf(FILE *, char *);
extern int	setvbuf(FILE *, char *, int, size_t);
 
extern int	fprintf(FILE *, const char *, ...);
 
extern int	fscanf(FILE *, const char *, ...);
 
extern int	printf(const char *, ...);
 
extern int	scanf(const char *, ...);
 
extern int	sprintf(char *, const char *, ...);
 
extern int	sscanf(const char *, const char *, ...);
extern int	vfprintf(FILE *, const char *, __va_list);
extern int	vprintf(const char *, __va_list);
extern int	vsprintf(char *, const char *, __va_list);
extern int	fgetc(FILE *);
extern char	*fgets(char *, int, FILE *);
extern int	fputc(int, FILE *);
extern int	fputs(const char *, FILE *);
extern int	getc(FILE *);
extern int	getchar(void);
extern char	*gets(char *);
extern int	putc(int, FILE *);
extern int	putchar(int);
extern int	puts(const char *);
extern int	ungetc(int, FILE *);
extern size_t	fread(void *, size_t, size_t, FILE *);
extern size_t	fwrite(const void *, size_t, size_t, FILE *);
extern int	fgetpos(FILE *, fpos_t *);
extern int	fseek(FILE *, long, int);
extern int	fsetpos(FILE *, const fpos_t *);
extern long	ftell(FILE *);
extern void	rewind(FILE *);
extern void	clearerr(FILE *);
extern int	feof(FILE *);
extern int	ferror(FILE *);
extern void	perror(const char *);

extern int	__filbuf(FILE *);
extern int	__flsbuf(int, FILE *);

 





extern FILE	*fdopen(int, const char *);
extern char	*ctermid(char *);
extern int	fileno(FILE *);



 




 




extern FILE	*popen(const char *, const char *);
extern char	*cuserid(char *);
extern char	*tempnam(const char *, const char *);
extern int	getopt(int, char *const *, const char *);

extern int	getsubopt(char **, char *const *, char **);

extern char	*optarg;
extern int	optind, opterr, optopt;
extern int	getw(FILE *);
extern int	putw(int, FILE *);
extern int	pclose(FILE *);





 











#define 	getc(p)		(--(p)->_cnt < 0 ? __filbuf(p) : (int)*(p)->_ptr++)
#define 	putc(x, p)	(--(p)->_cnt < 0 ? __flsbuf((unsigned char) (x), (p)) : (int)(*(p)->_ptr++ = (x)))







#define 	getchar()	getc(stdin)
#define 	putchar(x)	putc((x), stdout)
#define 	clearerr(p)	((void)((p)->_flag &= ~(_IOERR | _IOEOF)))
#define 	feof(p)		((p)->_flag & _IOEOF)
#define 	ferror(p)	((p)->_flag & _IOERR)




#define 	fileno(p)	((p)->_file)







}





 




#define _SYS_IEEEFP_H

#pragma ident	"@(#)ieeefp.h	2.7 94/11/09"


extern "C" {


 



enum fp_direction_type {	 
	fp_nearest	= 0,
	fp_tozero	= 1,
	fp_positive	= 2,
	fp_negative	= 3
};

enum fp_precision_type {	 
	fp_extended	= 0,
	fp_single	= 1,
	fp_double	= 2,
	fp_precision_3	= 3
};

enum fp_exception_type {	 
	fp_inexact	= 0,
	fp_division	= 1,
	fp_underflow	= 2,
	fp_overflow	= 3,
	fp_invalid	= 4
};

enum fp_trap_enable_type {	 
	fp_trap_inexact	= 0,
	fp_trap_division	= 1,
	fp_trap_underflow	= 2,
	fp_trap_overflow	= 3,
	fp_trap_invalid	= 4
};






enum fp_class_type {		 
	fp_zero		= 0,
	fp_subnormal	= 1,
	fp_normal	= 2,
	fp_infinity   	= 3,
	fp_quiet	= 4,
	fp_signaling	= 5
};


}















#define N_IEEE_EXCEPTION 5	

typedef int sigfpe_code_type;	 

typedef void (*sigfpe_handler_type)();	 

#define SIGFPE_DEFAULT (void (*)())0	
#define SIGFPE_IGNORE  (void (*)())1  	
#define SIGFPE_ABORT   (void (*)())2  	

extern sigfpe_handler_type sigfpe  (sigfpe_code_type, sigfpe_handler_type)  ;

 


typedef float single;			


#define _EXTENDED
typedef unsigned extended[3];


typedef long double quadruple;	 

typedef unsigned fp_exception_field_type;
				 



 


#define DECIMAL_STRING_LENGTH 512	

typedef char decimal_string[512 ];	
				 

typedef struct {
	enum fp_class_type fpclass;
	int	sign;
	int	exponent;
	decimal_string ds;	 


	int	more;		 


	int 	ndigits;	 


} decimal_record;

enum decimal_form {
	fixed_form,		 


	floating_form		 

};

typedef struct {
	enum fp_direction_type rd;	
				 
	enum decimal_form df;	 

	int ndigits;		 
} decimal_mode;

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

extern void single_to_decimal  (single *, decimal_mode *, decimal_record *,
				fp_exception_field_type *)  ;
extern void double_to_decimal  (double *, decimal_mode *, decimal_record *,
				fp_exception_field_type *)  ;
extern void extended_to_decimal  (extended *, decimal_mode *,
				decimal_record *, fp_exception_field_type *)  ;
extern void quadruple_to_decimal  (quadruple *, decimal_mode *,
				decimal_record *, fp_exception_field_type *)  ;

extern void decimal_to_single  (single *, decimal_mode *, decimal_record *,
				fp_exception_field_type *)  ;
extern void decimal_to_double  (double *, decimal_mode *, decimal_record *,
				fp_exception_field_type *)  ;
extern void decimal_to_extended  (extended *, decimal_mode *,
				decimal_record *, fp_exception_field_type *)  ;
extern void decimal_to_quadruple  (quadruple *, decimal_mode *,
				decimal_record *, fp_exception_field_type *)  ;

extern void string_to_decimal  (char **, int, int, decimal_record *,
				enum decimal_string_form *, char **)  ;
extern void func_to_decimal  (char **, int, int, decimal_record *,
				enum decimal_string_form *, char **,
				int (*)(void), int *, int (*)(int))  ;
extern void file_to_decimal  (char **, int, int, decimal_record *,
				enum decimal_string_form *, char **,
				FILE *, int *)  ;

extern char *seconvert  (single *, int, int *, int *, char *)  ;
extern char *sfconvert  (single *, int, int *, int *, char *)  ;
extern char *sgconvert  (single *, int, int, char *)  ;
extern char *econvert  (double, int, int *, int *, char *)  ;
extern char *fconvert  (double, int, int *, int *, char *)  ;
extern char *gconvert  (double, int, int, char *)  ;
extern char *qeconvert  (quadruple *, int, int *, int *, char *)  ;
extern char *qfconvert  (quadruple *, int, int *, int *, char *)  ;
extern char *qgconvert  (quadruple *, int, int, char *)  ;

extern char *ecvt  (double, int, int *, int *)  ;
extern char *fcvt  (double, int, int *, int *)  ;
extern char *gcvt  (double, int, char *)  ;

 



extern double atof  (const char *)  ;
extern double strtod  (const char *, char **)  ;


}








}




 
 



















#pragma interface

#define _String_h 1


 
 



















#pragma interface

#define _Regex_h 1







struct re_pattern_buffer;        
struct re_registers;

class Regex
{
private:

                     Regex(const Regex&) {}   
  void               operator = (const Regex&) {}  

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

  int                OK() const;   
};

 

 
 
 
 
 
 
 
 
 
 





struct StrRep                      
{
  unsigned short    len;          
  unsigned short    sz;           
  char              s[1];         
                                  
                                  
};

 

StrRep*     Salloc(StrRep*, const char*, int, int);
StrRep*     Scopy(StrRep*, const StrRep*);
StrRep*     Scat(StrRep*, const char*, int, const char*, int);
StrRep*     Scat(StrRep*, const char*, int,const char*,int, const char*,int);
StrRep*     Sprepend(StrRep*, const char*, int);
StrRep*     Sreverse(const StrRep*, StrRep*);
StrRep*     Supcase(const StrRep*, StrRep*);
StrRep*     Sdowncase(const StrRep*, StrRep*);
StrRep*     Scapitalize(const StrRep*, StrRep*);

 

class String;
class SubString;

class SubString
{
  friend class      String;
protected:

  String&           S;         
  unsigned short    pos;       
  unsigned short    len;       

  void              assign(const StrRep*, const char*, int = -1);
                    SubString(String& x, int p, int l);

public:
                    SubString(const SubString& x);  

 
 

                   ~SubString();

  SubString&        operator =  (const String&     y);
  SubString&        operator =  (const SubString&  y);
  SubString&        operator =  (const char* t);
  SubString&        operator =  (char        c);

 

  int               contains(char        c) const;
  int               contains(const String&     y) const;
  int               contains(const SubString&  y) const;
  int               contains(const char* t) const;
  int               contains(const Regex&       r) const;

 

  int               matches(const Regex&  r) const;

 

  friend ostream&   operator<<(ostream& s, const SubString& x);

 

  unsigned int      length() const;
  int               empty() const;
  const char*       chars() const;

  int               OK() const;

};


class String
{
  friend class      SubString;

protected:
  StrRep*           rep;    

 

  int               search(int, int, const char*, int = -1) const;
  int               search(int, int, char) const;
  int               match(int, int, int, const char*, int = -1) const;
  int               _gsub(const char*, int, const char* ,int);
  int               _gsub(const Regex&, const char*, int);
  SubString         _substr(int, int);

public:

 

                    String();
                    String(const String& x);
                    String(const SubString&  x);
                    String(const char* t);
                    String(const char* t, int len);
                    String(char c);

                    ~String();

  String&           operator =  (const String&     y);
  String&           operator =  (const char* y);
  String&           operator =  (char        c);
  String&           operator =  (const SubString&  y);

 

  String&           operator += (const String&     y);
  String&           operator += (const SubString&  y);
  String&           operator += (const char* t);
  String&           operator += (char        c);

  void              prepend(const String&     y);
  void              prepend(const SubString&  y);
  void              prepend(const char* t);
  void              prepend(char        c);


 
 

  friend inline void     cat(const String&, const String&, String&);
  friend inline void     cat(const String&, const SubString&, String&);
  friend inline void     cat(const String&, const char*, String&);
  friend inline void     cat(const String&, char, String&);

  friend inline void     cat(const SubString&, const String&, String&);
  friend inline void     cat(const SubString&, const SubString&, String&);
  friend inline void     cat(const SubString&, const char*, String&);
  friend inline void     cat(const SubString&, char, String&);

  friend inline void     cat(const char*, const String&, String&);
  friend inline void     cat(const char*, const SubString&, String&);
  friend inline void     cat(const char*, const char*, String&);
  friend inline void     cat(const char*, char, String&);

 
 
 

  friend inline void     cat(const String&,const String&, const String&,String&);
  friend inline void     cat(const String&,const String&,const SubString&,String&);
  friend inline void     cat(const String&,const String&, const char*, String&);
  friend inline void     cat(const String&,const String&, char, String&);
  friend inline void     cat(const String&,const SubString&,const String&,String&);
  inline friend void     cat(const String&,const SubString&,const SubString&,String&);
  friend inline void     cat(const String&,const SubString&, const char*, String&);
  friend inline void     cat(const String&,const SubString&, char, String&);
  friend inline void     cat(const String&,const char*, const String&,    String&);
  friend inline void     cat(const String&,const char*, const SubString&, String&);
  friend inline void     cat(const String&,const char*, const char*, String&);
  friend inline void     cat(const String&,const char*, char, String&);

  friend inline void     cat(const char*, const String&, const String&,String&);
  friend inline void     cat(const char*,const String&,const SubString&,String&);
  friend inline void     cat(const char*,const String&, const char*, String&);
  friend inline void     cat(const char*,const String&, char, String&);
  friend inline void     cat(const char*,const SubString&,const String&,String&);
  friend inline void     cat(const char*,const SubString&,const SubString&,String&);
  friend inline void     cat(const char*,const SubString&, const char*, String&);
  friend inline void     cat(const char*,const SubString&, char, String&);
  friend inline void     cat(const char*,const char*, const String&,    String&);
  friend inline void     cat(const char*,const char*, const SubString&, String&);
  friend inline void     cat(const char*,const char*, const char*, String&);
  friend inline void     cat(const char*,const char*, char, String&);


 

 

  int               index(char        c, int startpos = 0) const;
  int               index(const String&     y, int startpos = 0) const;
  int               index(const SubString&  y, int startpos = 0) const;
  int               index(const char* t, int startpos = 0) const;
  int               index(const Regex&      r, int startpos = 0) const;

 

  int               contains(char        c) const;
  int               contains(const String&     y) const;
  int               contains(const SubString&  y) const;
  int               contains(const char* t) const;
  int               contains(const Regex&      r) const;

 
 

  int               contains(char        c, int pos) const;
  int               contains(const String&     y, int pos) const;
  int               contains(const SubString&  y, int pos) const;
  int               contains(const char* t, int pos) const;
  int               contains(const Regex&      r, int pos) const;

 

  int               matches(char        c, int pos = 0) const;
  int               matches(const String&     y, int pos = 0) const;
  int               matches(const SubString&  y, int pos = 0) const;
  int               matches(const char* t, int pos = 0) const;
  int               matches(const Regex&      r, int pos = 0) const;

 

  int               freq(char        c) const;
  int               freq(const String&     y) const;
  int               freq(const SubString&  y) const;
  int               freq(const char* t) const;

 

 
 
 

  SubString         at(int         pos, int len);
  SubString         operator () (int         pos, int len);  

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


 

 
  void              del(int         pos, int len);

 

  void              del(const String&     y, int startpos = 0);
  void              del(const SubString&  y, int startpos = 0);
  void              del(const char* t, int startpos = 0);
  void              del(char        c, int startpos = 0);
  void              del(const Regex&      r, int startpos = 0);

 

  int               gsub(const String&     pat, const String&     repl);
  int               gsub(const SubString&  pat, const String&     repl);
  int               gsub(const char* pat, const String&     repl);
  int               gsub(const char* pat, const char* repl);
  int               gsub(const Regex&      pat, const String&     repl);

 

 

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

 

  friend inline String     reverse(const String& x);
  friend inline String     upcase(const String& x);
  friend inline String     downcase(const String& x);
  friend inline String     capitalize(const String& x);

 

  void              reverse();
  void              upcase();
  void              downcase();
  void              capitalize();

 

  char&             operator [] (int i);
  const char&       operator [] (int i) const;
  char              elem(int i) const;
  char              firstchar() const;
  char              lastchar() const;

 

                    operator const char*() const;
  const char*       chars() const;


 

  friend inline ostream&   operator<<(ostream& s, const String& x);
  friend ostream&   operator<<(ostream& s, const SubString& x);
  friend istream&   operator>>(istream& s, String& x);

  friend int        readline(istream& s, String& x,
                             char terminator = '\n',
                             int discard_terminator = 1);

 

  unsigned int      length() const;
  int               empty() const;

 
  void              alloc(int newsize);

 

  int               allocation() const;


  void     error(const char* msg) const;

  int               OK() const;
};

typedef String StrTmp;  

 

int        compare(const String&    x, const String&     y);
int        compare(const String&    x, const SubString&  y);
int        compare(const String&    x, const char* y);
int        compare(const SubString& x, const String&     y);
int        compare(const SubString& x, const SubString&  y);
int        compare(const SubString& x, const char* y);
int        fcompare(const String&   x, const String&     y);  

extern StrRep  _nilStrRep;
extern String _nilString;

 

inline unsigned int String::length() const {  return rep->len; }
inline int         String::empty() const { return rep->len == 0; }
inline const char* String::chars() const { return &(rep->s[0]); }
inline int         String::allocation() const { return rep->sz; }

inline unsigned int SubString::length() const { return len; }
inline int         SubString::empty() const { return len == 0; }
inline const char* SubString::chars() const { return &(S.rep->s[pos]); }


 

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

 

inline String& String::operator =  (const String& y)
{
  rep = Scopy(rep, y.rep);
  return *this;
}

inline String& String::operator=(const char* t)
{
  rep = Salloc(rep, t, -1, -1);
  return *this;
}

inline String& String::operator=(const SubString&  y)
{
  rep = Salloc(rep, y.chars(), y.length(), y.length());
  return *this;
}

inline String& String::operator=(char c)
{
  rep = Salloc(rep, &c, 1, 1);
  return *this;
}


inline SubString& SubString::operator = (const char* ys)
{
  assign(0, ys);
  return *this;
}

inline SubString& SubString::operator = (char ch)
{
  assign(0, &ch, 1);
  return *this;
}

inline SubString& SubString::operator = (const String& y)
{
  assign(y.rep, y.chars(), y.length());
  return *this;
}

inline SubString& SubString::operator = (const SubString& y)
{
  assign(y.S.rep, y.chars(), y.length());
  return *this;
}

 

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


 

inline String& String::operator +=(const String& y)
{
  cat(*this, y, *this);
  return *this;
}

inline String& String::operator +=(const SubString& y)
{
  cat(*this, y, *this);
  return *this;
}

inline String& String::operator += (const char* y)
{
  cat(*this, y, *this);
  return *this;
}

inline String& String:: operator +=(char y)
{
  cat(*this, y, *this);
  return *this;
}

 



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

 

inline char&  String::operator [] (int i)
{
  if (((unsigned)i) >= length()) error("invalid index");
  return rep->s[i];
}

inline const char&  String::operator [] (int i) const
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
  return S.search(pos, pos+len, c) >= 0;
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


 

inline SubString String::_substr(int first, int l)
{
  if (first < 0 || (unsigned)(first + l) > length() )
    return SubString(_nilString, 0, 0) ;
  else
    return SubString(*this, first, l);
}





#define booleen_hh

 
typedef int booleen;

const booleen VRAI=1;
const booleen FAUX=0;




#define valeur_bidon 12345678

#define fors(type, i, imin, imax) for(type i=imin; i< imax; i++)
#define fore(type, i, imin, imax) for(type i=imin; i<=imax; i++)

void verif_nb_param(int argc, char* argv[], int nb_params);

booleen f_exists(String nom);

 
template<class Type>
inline Type carre(const Type x)
{
  return x*x;
}

template<class Type>
inline Type cube(const Type x)
{
  return x*x*x;
}

template<class Type>
inline Type rac3(const Type x)
{
  if(fabs(x)<1E-5)
    return 0;
  else
    if(x>0)
      return exp(log(x)/3);
    else
      return -exp(log(-x)/3);
}

template<class Type>
inline Type min(const Type a,const Type b)
{
  return (a>b)?b:a;
}

template<class Type>
inline Type max(const Type a,const Type b)
{
  return (a<b)?b:a;
}

template<class Type>
inline Type min(const Type a,const Type b,const Type c)
{
  return min(a,min(b,c));
}

template<class Type>
inline Type max(const Type a,const Type b,const Type c)
{
  return max(a,max(b,c));
}

template <class Type>
inline void echange(Type& a,Type& b)
{
  Type t=a;
  a=b;
  b=t;
}

 
String i2S(int n,int l=0);

template <class Type>
inline booleen dans(Type x, Type a, Type b)
{
  return (a<=x) && (x<=b);
}

template <class Type>
inline Type mabs(Type x)
{
  return (x<0) ? -x : x ;
}

template <class Type>
inline booleen dans2(Type x, Type a, Type b)	   
   
{
  return mabs(x-(a+b)/2) <= mabs((a-b)/2) ;
}

template <class Type>
inline booleen proche(Type x, Type y, Type eps)
{
  return mabs(x-y)<eps;				   
}

template <class Type>
inline int arrondi(Type x)
{
  return int(x+0.5);
}

template<class Type>
unsigned char arrondi_ng(Type x)
{
  if((-0.5<x) && (x<255.5))
    return (unsigned char)(x+0.5);
  else
  {
    if(x<-3.0 || x>268.0)
      cerr<<"arrondi_ng : attention x= "<<x<<endl;
     
    if(x<0.0)
      return 0;
    else
      return 255;
  }
}

template<class Type>
unsigned char arrondi_ng_err(Type x)
{
  if((-0.5<x) && (x<255.5))
    return (unsigned char)(x+0.5);
  else
  {
    if(x<0.0)
      return 0;
    else
      return 255;
  }
}

inline int nb_diff2(int a,int b)		   
{
  if(a==b)
    return 1;
  else
    return 2;
}

inline int nb_diff3(int a,int b,int c)		   
{
  if(a==b || a==c)
    return nb_diff2(b,c);
  else
    return 1+nb_diff2(b,c);
}

inline int nb_diff4(int a,int b,int c,int d)	   
{
  if(a==b || a==c || a==d)
    return nb_diff3(b,c,d);
  else
    return 1+nb_diff3(b,c,d);
}

float echMSB(float a);

void plante();					   
void arrete();					   

void touche();					   

template<class Type>
void lis_param(istream& f, Type& param)
{
  f>>param;
  f.ignore(20000,'\n');				   
}

void lis_chaine(istream& s, String chaine);

template<class Type_dest, class Type_source>
void convert(Type_dest& dest, const Type_source& source)
{
  dest=source;
}




struct Vect2Dent
{
  int di;
  int dj;
};






struct depl2D
{
  double x;
  double y;
  depl2D() : x(0.0), y(0.0)
  { }
   
   
  depl2D(double xx, double yy) : x(xx), y(yy)
  { }
  double amplitude()
  {
    return max(mabs(x),mabs(y));
  }
};

ostream& operator<<(ostream& s, depl2D m);

class Mouvement
{
private:
  Mouvement* read_mv(istream& s);
protected:
  double t_x;
  double t_y;
  double centre_x;
  double centre_y;
public:
  Mouvement();
  Mouvement(double i_t_x, double i_t_y, double i_centre_x, double i_centre_y);
  virtual void Id()=0;
  virtual void centre(double x,double y)=0;
  virtual void applique(double& xx, double& yy, double x, double y) const=0;
  virtual void applique(float& xx, float& yy, float x, float y) const=0;
  virtual depl2D calc_depl(double x, double y)=0;
  double Centre_x() const
  {
    return centre_x;
  }
  double Centre_y() const
  {
    return centre_y;
  }

  virtual int nb_param() const=0;
   
   
  virtual void init_from_vect_d(const double param[])=0;
  virtual void init_vect_d(double param[]) const=0;
  virtual void init_from_vect_f(const float param[])=0;
  virtual void init_vect_f(float param[]) const=0;

  virtual Mouvement* clone() const=0;
  virtual String nom() const=0;
  virtual void dump(ostream& s) const=0;

  virtual booleen trop_grand(float seuil) const=0;
  booleen trop_grand2(float seuil, float seuil_t) const;
};

ostream& operator<<(ostream& s, const Mouvement& m);
Mouvement* read_mv(istream& s);




class AFF;

class LS : public Mouvement
{
protected:
  double k;
  double theta;
public:
  LS();
  LS(double i_t_x, double i_t_y, double i_k, double i_theta, double i_centre_x, double i_centre_y);
  LS(istream& s);
  LS(const AFF& aff);
  int nb_param() const;
  void update(double d_t_x, double d_t_y, double d_k, double d_theta);
  void init_from_vect_d(const double param[]);
  void init_vect_d(double param[]) const;
  void init_from_vect_f(const float param[]);
  void init_vect_f(float param[]) const;
  friend class AFF;
  void dump(ostream& s) const;
};

ostream& operator<<(ostream& s, const LS& m);




#define AFF_hh



class LS;

class AFF: public Mouvement
{
protected:
  double a;
  double b;
  double c;
  double d;
public:
  AFF();
  AFF(double i_t_x, double i_t_y,
      double i_a, double i_b, double i_c, double i_d,
      double i_centre_x, double i_centre_y);
  AFF(istream& s);
  AFF(const LS& ls);
  int nb_param() const;
  void init_from_vect_d(const double param[]);
  void init_vect_d(double param[]) const;
  void init_from_vect_f(const float param[]);
  void init_vect_f(float param[]) const;

  friend class LS;
  void dump(ostream& s) const;
};

ostream& operator<<(ostream& s, const AFF& m);









LS::LS() :
  k(0.0), theta(0.0)
{ }

LS::LS(double i_t_x, double i_t_y,
       double i_k, double i_theta,
       double i_centre_x, double i_centre_y) :
  Mouvement(i_t_x, i_t_y, i_centre_x, i_centre_y),
  k(i_k),
  theta(i_theta)
{ }

LS::LS(istream& s)
{
  s>>k>>theta>>t_x>>theta>>k>>t_y>>centre_x>>centre_y;
}

LS::LS(const AFF& aff): Mouvement(aff.t_x, aff.t_y, aff.centre_x, aff.centre_y)
{
  const double eps_k=1E-6;
  if(mabs(aff.a-aff.d)>eps_k)
  {
    cout<<"AFF_2_LS :  delta k < "<<eps_k<<endl;
    cout<<aff.a<<endl<<aff.d<<endl;
    plante();
  }
  else
    k=(aff.a+aff.d)/2;

  const double eps_theta=1E-6;
  if(mabs(aff.c+aff.b)>eps_theta)
  {
    cout<<"AFF_2_LS :  delta theta < "<<eps_theta<<endl;
    plante();
  }
  else
    theta=(aff.c-aff.b)/2;
}

int LS::nb_param() const
{
  return 4;
}

void LS::update(double d_t_x,
		double d_t_y,
		double d_k,
		double d_theta)
{
  t_x+=d_t_x;
  t_y+=d_t_y;
  k+=d_k;
  theta+=d_theta;
}

void LS::init_from_vect_d(const double param[])
{
  t_x=param[0];
  t_y=param[1];
  k=param[2];
  theta=param[3];
}

void LS::init_vect_d(double param[]) const
{
  param[0]=t_x;
  param[1]=t_y;
  param[2]=k;
  param[3]=theta;
}

void LS::init_from_vect_f(const float param[])
{
  t_x=param[0];
  t_y=param[1];
  k=param[2];
  theta=param[3];
}

void LS::init_vect_f(float param[]) const
{
  param[0]=t_x;
  param[1]=t_y;
  param[2]=k;
  param[3]=theta;
}

 
 
 
 
 
 
 
 

 
 
 
 

void LS::dump(ostream& s) const
{
  const int largeur=14;
  s.setf(ios::left,ios::adjustfield);
  s<<nom()<<endl
   <<setw(largeur)<<k<<setw(largeur)<<-theta<<setw(largeur)<<t_x<<endl
   <<setw(largeur)<<theta<<setw(largeur)<<k<<setw(largeur)<<t_y<<endl
   <<centre_x<<' '<<centre_y<<endl;
}

ostream& operator<<(ostream& s, const LS& m)
{
  m.dump(s);
  return s;
}
