// { dg-do assemble  }
// { dg-options "" }
// This test case caused the compiler to abort at one point in time.
// prms-id: 811

class ostream; class streambuf;

typedef long streamoff, streampos;

struct _ios_fields {  
    streambuf *_strbuf;
    ostream* _tie;
    int _width;
    unsigned long _flags;
    char _fill;
    unsigned char _state;
    unsigned short _precision;
};


enum state_value { _good = 0, _eof = 1,  _fail = 2, _bad  = 4 };
enum open_mode { input=1, output=2, append=8 };


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
	   dont_close=0x80000000  
	   };

    ostream* tie() const { return _tie; }
    ostream* tie(ostream* val) { ostream* save=_tie; _tie=val; return save; }

     
    char fill() const { return _fill; }
    char fill(char newf) { char oldf = _fill; _fill = newf; return oldf; }
    unsigned long flags() const { return _flags; }
    unsigned long flags(unsigned long new_val) {
	unsigned long old_val = _flags; _flags = new_val; return old_val; }
    unsigned short precision() const { return _precision; }
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
    int width() const { return _width; }
    int width(long val) { long save = _width; _width = val; return save; }

    static const unsigned long basefield;
    static const unsigned long adjustfield;
    static const unsigned long floatfield;

    streambuf* rdbuf() const { return _strbuf; }
    void clear(int state = 0) { _state = state; }
    int good() const { return _state == 0; }
    int eof() const { return _state & ios::eofbit; }
    int fail() const { return _state & (ios::badbit|ios::failbit); }
    int bad() const { return _state & ios::badbit; }
    int rdstate() const { return _state; }
    void set(int flag) { _state |= flag; }
    operator void*() const { return fail() ? (void*)0 : (void*)this; }
    int operator!() const { return fail(); }


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


 
 
 
 
 















//# 168 "/usr/latest/lib/g++-include/streambuf.h" 3


struct __streambuf {
     
    int _flags;		 
    char* _gptr;	 
    char* _egptr;	 
    char* _eback;	 
    char* _pbase;	 
    char* _pptr;	 
    char* _epptr;	 
    char* _base;	 
    char* _ebuf;	 
    struct streambuf *_chain;




};

struct streambuf : private __streambuf {
    friend class ios;
    friend class istream;
    friend class ostream;
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
    static void flush_all_linebuffered();  
    virtual int underflow();  
    virtual int overflow(int c = (-1) );  
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
    long sgetline(char* buf, int  n, char delim, int putback_delim);
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
    long   _offset;
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
    virtual int sync();
  protected:  
    virtual int pbackfail(int c);
    int is_reading() { return eback() != egptr(); }
    char* cur_ptr() { return is_reading() ?  gptr() : pptr(); }
     
    char* file_ptr() { return _fb._save_gptr ? _fb._save_egptr : egptr(); }
    int do_flush();
     
    virtual int sys_read(char* buf, int  size);
    virtual long   sys_seek(long  , _seek_dir);
    virtual long sys_write(const void*, long);
    virtual int sys_stat(void*);  
    virtual int sys_close();
};


inline int ios::readable() { return rdbuf()->_flags & 4 ; }
inline int ios::writable() { return rdbuf()->_flags & 8 ; }
inline int ios::is_open() {return rdbuf()->_flags & 4 +8 ;}




//# 25 "/usr/latest/lib/g++-include/iostream.h" 2 3


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
    ostream(streambuf* sb, ostream* tied=__null );
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

extern ostream& operator<<(ostream&, char c);
inline ostream& operator<<(ostream& os, unsigned char c)
{ return os << (char)c; }
 
extern ostream& operator<<(ostream&, const char *s);
inline ostream& operator<<(ostream& os, const unsigned char *s)
{ return os << (const char*)s; }
 
 
extern ostream& operator<<(ostream&, void *p);
extern ostream& operator<<(ostream&, int n);
extern ostream& operator<<(ostream&, long n);
extern ostream& operator<<(ostream&, unsigned int n);
extern ostream& operator<<(ostream&, unsigned long n);
inline ostream& operator<<(ostream& os, short n) {return os << (int)n;}
inline ostream& operator<<(ostream& os, unsigned short n)
{return os << (unsigned int)n;}
extern ostream& operator<<(ostream&, float n);
extern ostream& operator<<(ostream&, double n);
inline ostream& operator<<(ostream& os, __omanip func) { return (*func)(os); }
extern ostream& operator<<(ostream&, streambuf*);

class istream : public ios
{
    int  _gcount;
  public:
    istream();
    istream(streambuf* sb, ostream*tied=__null );
    ~istream();
    streambuf* istreambuf() const { return _strbuf; }
    istream& get(char& c);
    istream& get(unsigned char& c);
    istream& read(char *ptr, int n);
    istream& read(unsigned char *ptr, int n) { return read((char*)ptr, n); }
    istream& read(void *ptr, int n) { return read((char*)ptr, n); }
     
    istream& getline(char* ptr, int len, char delim = '\n');
    istream& get(char* ptr, int len, char delim = '\n');
    istream& gets(char **s, char delim = '\n');
    int ipfx(int need) {
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie && (need == 0 || rdbuf()->in_avail() < need)) _tie->flush();
	if (!need && (flags() & ios::skipws) && !ws(*this)) return 0;
	return 1;
    }
    int ipfx0() {  
	if (!good()) { set(ios::failbit); return 0; }
	if (_tie) _tie->flush();
	if ((flags() & ios::skipws) && !ws(*this)) return 0;
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
    int  gcount() { return _gcount; }
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

    istream& unget(char ch) { return putback(ch); }
    int skip(int i);

};

extern istream& operator>>(istream&, char*);
inline istream& operator>>(istream& is, unsigned char* p)
{ return is >> (char*)p; }
 
extern istream& operator>>(istream&, char& c);
extern istream& operator>>(istream&, unsigned char& c);
 
extern istream& operator>>(istream&, int&);
extern istream& operator>>(istream&, long&);
extern istream& operator>>(istream&, short&);
extern istream& operator>>(istream&, unsigned int&);
extern istream& operator>>(istream&, unsigned long&);
extern istream& operator>>(istream&, unsigned short&);
extern istream& operator>>(istream&, float&);
extern istream& operator>>(istream&, double&);
inline istream& operator>>(istream& is, __imanip func) { return (*func)(is); }

inline ostream& ostream::put(char c) { _strbuf->sputc(c); return *this; }

class iostream : public ios {
    int  _gcount;
  public:
    iostream();
    iostream(streambuf* sb, ostream*tied=__null );
    operator istream&() { return *(istream*)this; }
    operator ostream&() { return *(ostream*)this; }
    ~iostream();
     
    istream& get(char& c) { return ((istream*)this)->get(c); }
    istream& get(unsigned char& c) { return ((istream*)this)->get(c); }
    istream& read(char *ptr, int n) { return ((istream*)this)->read(ptr, n); }
    istream& read(unsigned char *ptr, int n)
	{ return ((istream*)this)->read((char*)ptr, n); }
    istream& read(void *ptr, int n)
	{ return ((istream*)this)->read((char*)ptr, n); }
    istream& getline(char* ptr, int len, char delim = '\n')
	{ return ((istream*)this)->getline(ptr, len, delim); }
    istream& get(char* ptr, int len, char delim = '\n')
	{ return ((istream*)this)->get(ptr, len, delim); }
    istream& gets(char **s, char delim = '\n')
	{ return ((istream*)this)->gets(s, delim); }
    istream& ignore(int n=1, int delim = (-1) )
	{ return ((istream*)this)->ignore(n, delim); }
    int ipfx(int need) { return ((istream*)this)->ipfx(need); }
    int ipfx0()  { return ((istream*)this)->ipfx0(); }
    int ipfx1()  { return ((istream*)this)->ipfx1(); }
    int get() { return _strbuf->sbumpc(); }
    int peek() { return ipfx1() ? _strbuf->sgetc() : (-1) ; }
    int  gcount() { return _gcount; }
    istream& putback(char ch) { return ((istream*)this)->putback(ch); }
    istream& unget() { return ((istream*)this)->unget(); }
    istream& seekg(streampos pos) { return ((istream*)this)->seekg(pos); }
    istream& seekg(streamoff off, _seek_dir dir)
	{ return ((istream*)this)->seekg(off, dir); }
    streampos tellg() { return ((istream*)this)->tellg(); }

    istream& unget(char ch) { return putback(ch); }


     
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
extern ostream cout, cerr, clog;  

struct Iostream_init { } ;   

inline ios& dec(ios& i)
{ i.setf(ios::dec, ios::dec|ios::hex|ios::oct); return i; }
inline ios& hex(ios& i)
{ i.setf(ios::hex, ios::dec|ios::hex|ios::oct); return i; }
inline ios& oct(ios& i)
{ i.setf(ios::oct, ios::dec|ios::hex|ios::oct); return i; }


//# 7 "/usr/latest/lib/g++-include/stream.h" 2 3


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


//# 9 "test.C" 2


class Y {
public:
    Y() {}
    virtual const char *stringify() = 0;
    virtual char *stringify2() const = 0; // { dg-error "" } 
};

class X: public Y {
public:
    X(): Y() {}
    char *stringify();		// { dg-error "" } ok
    const char *stringify2() const;  // { dg-error "" } ok
};

char *
X::stringify() const
{ // { dg-error "" } ok
    return "stringify";
}

const char *
X::stringify2()
{ // { dg-error "" } ok
    return "stringify2";
}

main()
{
    X x;
    Y& y = x;

    cout << "x\n";
    cout << x.stringify() << '\n';
    cout << x.stringify2() << '\n';

    cout << "y\n";
    cout << y.stringify() << '\n';
    cout << y.stringify2() << '\n';
}
