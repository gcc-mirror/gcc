// Special g++ Options:
// prms-id: 700

//# 1 "../../../../libg++/etc/benchmarks/dhrystone.cc"

























//# 1 "../../../../libg++/etc/benchmarks/Int.h" 1











































































class Int
{
protected:
  int          rep;



public:
               Int ();
               Int (const int  b);
               Int (const Int& b);
              ~Int();

               operator int() const;

  inline virtual  int   val() const;

  inline virtual  void  operator  = (const int);
  inline virtual  void  operator  = (const Int&);

  inline virtual  void  negate();
  inline virtual  void  complement();
  inline virtual  void  operator ++ ();
  inline virtual  void  operator -- ();

  inline virtual  void  operator += (const Int &  );
  inline virtual  void  operator -= (const Int &  );
  inline virtual  void  operator *= (const Int &  );
  inline virtual  void  operator /= (const Int &  );
  inline virtual  void  operator %= (const Int &  );
  inline virtual  void  operator |= (const Int &  );
  inline virtual  void  operator &= (const Int &  );
  inline virtual  void  operator ^= (const Int &  );
  inline virtual  void  operator <<=(const Int &  );
  inline virtual  void  operator >>=(const Int &  );


  inline virtual  void  operator += (const int);
  inline virtual  void  operator -= (const int);
  inline virtual  void  operator *= (const int);
  inline virtual  void  operator /= (const int);
  inline virtual  void  operator %= (const int);
  inline virtual  void  operator |= (const int);
  inline virtual  void  operator &= (const int);
  inline virtual  void  operator ^= (const int);
  inline virtual  void  operator <<=(const int);
  inline virtual  void  operator >>=(const int);


};

inline  int  Int::val() const { return rep; }
inline       Int::operator int() const { return val(); }

inline       Int::Int () :rep(0) {}
inline       Int::Int (const int  b) :rep(b) {}
inline       Int::Int (const Int& b) :rep(b.Int::val()) {}
inline       Int::~Int() {}

inline  void  Int::operator  = (const int  b)
{ rep = b;  ; }
inline  void  Int::operator  = (const Int&  b)
{ rep = b.Int::val();  ; }

inline  void  Int::complement()
{ rep = ~rep;  ; }
inline  void  Int::negate()
{ rep = -rep;  ; }
inline  void  Int::operator ++ ()
{ ++rep;  ; }
inline  void  Int::operator -- ()
{ --rep;  ; }

inline  void  Int::operator += (const Int &   b)
{ rep += b.Int::val();  ; }
inline  void  Int::operator -= (const Int &   b)
{ rep -= b.Int::val();  ; }
inline  void  Int::operator *= (const Int &   b)
{ rep *= b.Int::val();  ; }
inline  void  Int::operator /= (const Int &   b)
{ rep /= b.Int::val();  ; }
inline  void  Int::operator %= (const Int &   b)
{ rep %= b.Int::val();  ; }
inline  void  Int::operator |= (const Int &   b)
{ rep |= b.Int::val();  ; }
inline  void  Int::operator &= (const Int &   b)
{ rep &= b.Int::val();  ; }
inline  void  Int::operator ^= (const Int &   b)
{ rep ^= b.Int::val();  ; }
inline  void  Int::operator <<=(const Int &   b)
{ rep <<= b.Int::val();  ; }
inline  void  Int::operator >>=(const Int &   b)
{ rep >>= b.Int::val();  ; }



inline  void  Int::operator += (const int b)
{ rep += b;  ; }
inline  void  Int::operator -= (const int b)
{ rep -= b;  ; }
inline  void  Int::operator *= (const int b)
{ rep *= b;  ; }
inline  void  Int::operator /= (const int b)
{ rep /= b;  ; }
inline  void  Int::operator %= (const int b)
{ rep %= b;  ; }
inline  void  Int::operator |= (const int b)
{ rep |= b;  ; }
inline  void  Int::operator &= (const int b)
{ rep &= b;  ; }
inline  void  Int::operator ^= (const int b)
{ rep ^= b;  ; }
inline  void  Int::operator <<=(const int b)
{ rep <<= b;  ; }
inline  void  Int::operator >>=(const int b)
{ rep >>= b;  ; }


inline  int& operator  = (int& a,  const Int &   b)
{ a = b.Int::val(); return a;}	// WARNING - 
inline  int& operator += (int& a,  const Int &   b)
{ a += b.Int::val(); return a; }
inline  int& operator -= (int& a,  const Int &   b)
{ a -= b.Int::val(); return a;}
inline  int& operator *= (int& a,  const Int &   b)
{ a *= b.Int::val(); return a;}
inline  int& operator /= (int& a,  const Int &   b)
{ a /= b.Int::val(); return a;}
inline  int& operator %= (int& a,  const Int &   b)
{ a %= b.Int::val(); return a;}
inline  int& operator |= (int& a,  const Int &   b)
{ a |= b.Int::val(); return a;}
inline  int& operator &= (int& a,  const Int &   b)
{ a &= b.Int::val(); return a;}
inline  int& operator ^= (int& a,  const Int &   b)
{ a ^= b.Int::val(); return a;}
inline  int& operator <<=(int& a,  const Int &   b)
{ a <<= b.Int::val(); return a;}
inline  int& operator >>=(int& a,  const Int &   b)
{ a >>= b.Int::val(); return a;}



//# 289 "../../../../libg++/etc/benchmarks/Int.h"


inline  Int  operator -  (const Int &   a) return r(a)
{ r.negate();  }
inline  Int  operator ~  (const Int &   a) return r(a)
{ r.complement();  }

inline  Int  operator +  (const Int &   a, const Int &   b) return r(a)
{ r += b.Int::val();  }
inline  Int  operator -  (const Int &   a, const Int &   b) return r(a)
{ r -= b.Int::val();  }
inline  Int  operator *  (const Int &   a, const Int &   b) return r(a)
{ r *= b.Int::val();  }
inline  Int  operator /  (const Int &   a, const Int &   b) return r(a)
{ r /= b.Int::val();  }
inline  Int  operator %  (const Int &   a, const Int &   b) return r(a)
{ r %= b.Int::val();  }
inline  Int  operator << (const Int &   a, const Int &   b) return r(a)
{ r <<= b.Int::val();  }
inline  Int  operator >> (const Int &   a, const Int &   b) return r(a)
{ r >>= b.Int::val();  }
inline  Int  operator &  (const Int &   a, const Int &   b) return r(a)
{ r &= b.Int::val();  }
inline  Int  operator |  (const Int &   a, const Int &   b) return r(a)
{ r |= b.Int::val();  }
inline  Int  operator ^  (const Int &   a, const Int &   b) return r(a)
{ r ^= b.Int::val();  }

inline  Int  operator +  (const Int &   a, const int b) return r(a)
{ r += b;  }
inline  Int  operator -  (const Int &   a, const int b) return r(a)
{ r -= b;  }
inline  Int  operator *  (const Int &   a, const int b) return r(a)
{ r *= b;  }
inline  Int  operator /  (const Int &   a, const int b) return r(a)
{ r /= b;  }
inline  Int  operator %  (const Int &   a, const int b) return r(a)
{ r %= b;  }
inline  Int  operator << (const Int &   a, const int b) return r(a)
{ r <<= b;  }
inline  Int  operator >> (const Int &   a, const int b) return r(a)
{ r >>= b;  }
inline  Int  operator &  (const Int &   a, const int b) return r(a)
{ r &= b;  }
inline  Int  operator |  (const Int &   a, const int b) return r(a)
{ r |= b;  }
inline  Int  operator ^  (const Int &   a, const int b) return r(a)
{ r ^= b;  }

inline  Int  operator +  (const int a, const Int &   b) return r(a)
{ r += b.Int::val();  }
inline  Int  operator -  (const int a, const Int &   b) return r(a)
{ r -= b.Int::val();  }
inline  Int  operator *  (const int a, const Int &   b) return r(a)
{ r *= b.Int::val();  }
inline  Int  operator /  (const int a, const Int &   b) return r(a)
{ r /= b.Int::val();  }
inline  Int  operator %  (const int a, const Int &   b) return r(a)
{ r %= b.Int::val();  }
inline  Int  operator << (const int a, const Int &   b) return r(a)
{ r <<= b.Int::val();  }
inline  Int  operator >> (const int a, const Int &   b) return r(a)
{ r >>= b.Int::val();  }
inline  Int  operator &  (const int a, const Int &   b) return r(a)
{ r &= b.Int::val();  }
inline  Int  operator |  (const int a, const Int &   b) return r(a)
{ r |= b.Int::val();  }
inline  Int  operator ^  (const int a, const Int &   b) return r(a)
{ r ^= b.Int::val();  }



inline  int  operator !  (const Int &   a) { return !a.Int::val(); }

inline  int  operator == (const Int &   a, const Int &   b)
{ return a.Int::val() == b.Int::val(); }
inline  int  operator != (const Int &   a, const Int &   b)
{ return a.Int::val() != b.Int::val(); }
inline  int  operator <  (const Int &   a, const Int &   b)
{ return a.Int::val() <  b.Int::val(); }
inline  int  operator <= (const Int &   a, const Int &   b)
{ return a.Int::val() <= b.Int::val(); }
inline  int  operator >  (const Int &   a, const Int &   b)
{ return a.Int::val() >  b.Int::val(); }
inline  int  operator >= (const Int &   a, const Int &   b)
{ return a.Int::val() >= b.Int::val(); }

inline  int  operator == (const Int &   a, const int b)
{ return a.Int::val() == b; }
inline  int  operator != (const Int &   a, const int b)
{ return a.Int::val() != b; }
inline  int  operator <  (const Int &   a, const int b)
{ return a.Int::val() <  b; }
inline  int  operator <= (const Int &   a, const int b)
{ return a.Int::val() <= b; }
inline  int  operator >  (const Int &   a, const int b)
{ return a.Int::val() >  b; }
inline  int  operator >= (const Int &   a, const int b)
{ return a.Int::val() >= b; }

inline  int  operator == (const int a, const Int &   b)
{ return a == b.Int::val(); }
inline  int  operator != (const int a, const Int &   b)
{ return a != b.Int::val(); }
inline  int  operator <  (const int a, const Int &   b)
{ return a <  b.Int::val(); }
inline  int  operator <= (const int a, const Int &   b)
{ return a <= b.Int::val(); }
inline  int  operator >  (const int a, const Int &   b)
{ return a >  b.Int::val(); }
inline  int  operator >= (const int a, const Int &   b)
{ return a >= b.Int::val(); }



//# 26 "../../../../libg++/etc/benchmarks/dhrystone.cc" 2

//# 1 "../../../../libg++/etc/benchmarks/Char.h" 1













































































class Char
{
protected:
  char          rep;



public:
               Char ();
               Char (const char  b);
               Char (const Char& b);
              ~Char();

               operator char() const;

  inline virtual  char   val() const;

  inline virtual  void  operator  = (const char);
  inline virtual  void  operator  = (const Char&);

  inline virtual  void  negate();
  inline virtual  void  complement();
  inline virtual  void  operator ++ ();
  inline virtual  void  operator -- ();

  inline virtual  void  operator += (const Char &  );
  inline virtual  void  operator -= (const Char &  );
  inline virtual  void  operator *= (const Char &  );
  inline virtual  void  operator /= (const Char &  );
  inline virtual  void  operator %= (const Char &  );
  inline virtual  void  operator |= (const Char &  );
  inline virtual  void  operator &= (const Char &  );
  inline virtual  void  operator ^= (const Char &  );
  inline virtual  void  operator <<=(const Char &  );
  inline virtual  void  operator >>=(const Char &  );


  inline virtual  void  operator += (const char);
  inline virtual  void  operator -= (const char);
  inline virtual  void  operator *= (const char);
  inline virtual  void  operator /= (const char);
  inline virtual  void  operator %= (const char);
  inline virtual  void  operator |= (const char);
  inline virtual  void  operator &= (const char);
  inline virtual  void  operator ^= (const char);
  inline virtual  void  operator <<=(const char);
  inline virtual  void  operator >>=(const char);


};

inline  char  Char::val() const { return rep; }
inline       Char::operator char() const { return val(); }

inline       Char::Char () :rep(0) {}
inline       Char::Char (const char  b) :rep(b) {}
inline       Char::Char (const Char& b) :rep(b.Char::val()) {}
inline       Char::~Char() {}

inline  void  Char::operator  = (const char  b)
{ rep = b;  ; }
inline  void  Char::operator  = (const Char&  b)
{ rep = b.Char::val();  ; }

inline  void  Char::complement()
{ rep = ~rep;  ; }
inline  void  Char::negate()
{ rep = -rep;  ; }
inline  void  Char::operator ++ ()
{ ++rep;  ; }
inline  void  Char::operator -- ()
{ --rep;  ; }

inline  void  Char::operator += (const Char &   b)
{ rep += b.Char::val();  ; }
inline  void  Char::operator -= (const Char &   b)
{ rep -= b.Char::val();  ; }
inline  void  Char::operator *= (const Char &   b)
{ rep *= b.Char::val();  ; }
inline  void  Char::operator /= (const Char &   b)
{ rep /= b.Char::val();  ; }
inline  void  Char::operator %= (const Char &   b)
{ rep %= b.Char::val();  ; }
inline  void  Char::operator |= (const Char &   b)
{ rep |= b.Char::val();  ; }
inline  void  Char::operator &= (const Char &   b)
{ rep &= b.Char::val();  ; }
inline  void  Char::operator ^= (const Char &   b)
{ rep ^= b.Char::val();  ; }
inline  void  Char::operator <<=(const Char &   b)
{ rep <<= b.Char::val();  ; }
inline  void  Char::operator >>=(const Char &   b)
{ rep >>= b.Char::val();  ; }



inline  void  Char::operator += (const char b)
{ rep += b;  ; }
inline  void  Char::operator -= (const char b)
{ rep -= b;  ; }
inline  void  Char::operator *= (const char b)
{ rep *= b;  ; }
inline  void  Char::operator /= (const char b)
{ rep /= b;  ; }
inline  void  Char::operator %= (const char b)
{ rep %= b;  ; }
inline  void  Char::operator |= (const char b)
{ rep |= b;  ; }
inline  void  Char::operator &= (const char b)
{ rep &= b;  ; }
inline  void  Char::operator ^= (const char b)
{ rep ^= b;  ; }
inline  void  Char::operator <<=(const char b)
{ rep <<= b;  ; }
inline  void  Char::operator >>=(const char b)
{ rep >>= b;  ; }


inline  char& operator  = (char& a,  const Char &   b)
{ a = b.Char::val(); return a;}	// WARNING - 
inline  char& operator += (char& a,  const Char &   b)
{ a += b.Char::val(); return a; }
inline  char& operator -= (char& a,  const Char &   b)
{ a -= b.Char::val(); return a;}
inline  char& operator *= (char& a,  const Char &   b)
{ a *= b.Char::val(); return a;}
inline  char& operator /= (char& a,  const Char &   b)
{ a /= b.Char::val(); return a;}
inline  char& operator %= (char& a,  const Char &   b)
{ a %= b.Char::val(); return a;}
inline  char& operator |= (char& a,  const Char &   b)
{ a |= b.Char::val(); return a;}
inline  char& operator &= (char& a,  const Char &   b)
{ a &= b.Char::val(); return a;}
inline  char& operator ^= (char& a,  const Char &   b)
{ a ^= b.Char::val(); return a;}
inline  char& operator <<=(char& a,  const Char &   b)
{ a <<= b.Char::val(); return a;}
inline  char& operator >>=(char& a,  const Char &   b)
{ a >>= b.Char::val(); return a;}



//# 291 "../../../../libg++/etc/benchmarks/Char.h"


inline  Char  operator -  (const Char &   a) return r(a)
{ r.negate();  }
inline  Char  operator ~  (const Char &   a) return r(a)
{ r.complement();  }

inline  Char  operator +  (const Char &   a, const Char &   b) return r(a)
{ r += b.Char::val();  }
inline  Char  operator -  (const Char &   a, const Char &   b) return r(a)
{ r -= b.Char::val();  }
inline  Char  operator *  (const Char &   a, const Char &   b) return r(a)
{ r *= b.Char::val();  }
inline  Char  operator /  (const Char &   a, const Char &   b) return r(a)
{ r /= b.Char::val();  }
inline  Char  operator %  (const Char &   a, const Char &   b) return r(a)
{ r %= b.Char::val();  }
inline  Char  operator << (const Char &   a, const Char &   b) return r(a)
{ r <<= b.Char::val();  }
inline  Char  operator >> (const Char &   a, const Char &   b) return r(a)
{ r >>= b.Char::val();  }
inline  Char  operator &  (const Char &   a, const Char &   b) return r(a)
{ r &= b.Char::val();  }
inline  Char  operator |  (const Char &   a, const Char &   b) return r(a)
{ r |= b.Char::val();  }
inline  Char  operator ^  (const Char &   a, const Char &   b) return r(a)
{ r ^= b.Char::val();  }

inline  Char  operator +  (const Char &   a, const char b) return r(a)
{ r += b;  }
inline  Char  operator -  (const Char &   a, const char b) return r(a)
{ r -= b;  }
inline  Char  operator *  (const Char &   a, const char b) return r(a)
{ r *= b;  }
inline  Char  operator /  (const Char &   a, const char b) return r(a)
{ r /= b;  }
inline  Char  operator %  (const Char &   a, const char b) return r(a)
{ r %= b;  }
inline  Char  operator << (const Char &   a, const char b) return r(a)
{ r <<= b;  }
inline  Char  operator >> (const Char &   a, const char b) return r(a)
{ r >>= b;  }
inline  Char  operator &  (const Char &   a, const char b) return r(a)
{ r &= b;  }
inline  Char  operator |  (const Char &   a, const char b) return r(a)
{ r |= b;  }
inline  Char  operator ^  (const Char &   a, const char b) return r(a)
{ r ^= b;  }

inline  Char  operator +  (const char a, const Char &   b) return r(a)
{ r += b.Char::val();  }
inline  Char  operator -  (const char a, const Char &   b) return r(a)
{ r -= b.Char::val();  }
inline  Char  operator *  (const char a, const Char &   b) return r(a)
{ r *= b.Char::val();  }
inline  Char  operator /  (const char a, const Char &   b) return r(a)
{ r /= b.Char::val();  }
inline  Char  operator %  (const char a, const Char &   b) return r(a)
{ r %= b.Char::val();  }
inline  Char  operator << (const char a, const Char &   b) return r(a)
{ r <<= b.Char::val();  }
inline  Char  operator >> (const char a, const Char &   b) return r(a)
{ r >>= b.Char::val();  }
inline  Char  operator &  (const char a, const Char &   b) return r(a)
{ r &= b.Char::val();  }
inline  Char  operator |  (const char a, const Char &   b) return r(a)
{ r |= b.Char::val();  }
inline  Char  operator ^  (const char a, const Char &   b) return r(a)
{ r ^= b.Char::val();  }



inline  char  operator !  (const Char &   a) { return !a.Char::val(); }

inline  char  operator == (const Char &   a, const Char &   b)
{ return a.Char::val() == b.Char::val(); }
inline  char  operator != (const Char &   a, const Char &   b)
{ return a.Char::val() != b.Char::val(); }
inline  char  operator <  (const Char &   a, const Char &   b)
{ return a.Char::val() <  b.Char::val(); }
inline  char  operator <= (const Char &   a, const Char &   b)
{ return a.Char::val() <= b.Char::val(); }
inline  char  operator >  (const Char &   a, const Char &   b)
{ return a.Char::val() >  b.Char::val(); }
inline  char  operator >= (const Char &   a, const Char &   b)
{ return a.Char::val() >= b.Char::val(); }

inline  char  operator == (const Char &   a, const char b)
{ return a.Char::val() == b; }
inline  char  operator != (const Char &   a, const char b)
{ return a.Char::val() != b; }
inline  char  operator <  (const Char &   a, const char b)
{ return a.Char::val() <  b; }
inline  char  operator <= (const Char &   a, const char b)
{ return a.Char::val() <= b; }
inline  char  operator >  (const Char &   a, const char b)
{ return a.Char::val() >  b; }
inline  char  operator >= (const Char &   a, const char b)
{ return a.Char::val() >= b; }

inline  char  operator == (const char a, const Char &   b)
{ return a == b.Char::val(); }
inline  char  operator != (const char a, const Char &   b)
{ return a != b.Char::val(); }
inline  char  operator <  (const char a, const Char &   b)
{ return a <  b.Char::val(); }
inline  char  operator <= (const char a, const Char &   b)
{ return a <= b.Char::val(); }
inline  char  operator >  (const char a, const Char &   b)
{ return a >  b.Char::val(); }
inline  char  operator >= (const char a, const Char &   b)
{ return a >= b.Char::val(); }



//# 27 "../../../../libg++/etc/benchmarks/dhrystone.cc" 2



























































































































































































































































































//# 1 "/giga/hgs/lib/g++-include/sys/types.h" 1


//# 1 "/giga/hgs/lib/g++-include/stddef.h" 1

extern "C" {
//# 1 "/giga/hgs/lib/gcc/sun4/cygnus-1.96/include/stddef.h" 1






























typedef int ptrdiff_t;





















typedef int size_t;





















typedef short unsigned int wchar_t;



















//# 3 "/giga/hgs/lib/g++-include/stddef.h" 2

}
//# 73 "/giga/hgs/lib/g++-include/stddef.h"

//# 3 "/giga/hgs/lib/g++-include/sys/types.h" 2




extern "C"
{


























//# 1 "/usr/include/sys/types.h" 1















//# 1 "/usr/include/sys/stdtypes.h" 1













typedef	int		sigset_t;

typedef	unsigned int	speed_t;
typedef	unsigned long	tcflag_t;
typedef	unsigned char	cc_t;
typedef	int		pid_t;

typedef	unsigned short	mode_t;
typedef	short		nlink_t;

typedef	long		clock_t;
typedef	long		time_t;

typedef	int		size_t;
typedef int		ptrdiff_t;

typedef	unsigned short	wchar_t;


//# 16 "/usr/include/sys/types.h" 2



//# 1 "/usr/include/sys/sysmacros.h" 1



















//# 19 "/usr/include/sys/types.h" 2





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







//# 113 "/usr/include/sys/types.h"



//# 35 "/giga/hgs/lib/g++-include/sys/types.h" 2

































}




//# 310 "../../../../libg++/etc/benchmarks/dhrystone.cc" 2

//# 1 "/giga/hgs/lib/g++-include/sys/times.h" 1
//# 1 "/giga/hgs/lib/g++-include/time.h" 1





//# 1 "/giga/hgs/lib/g++-include/stddef.h" 1

extern "C" {
//# 1 "/giga/hgs/lib/gcc/sun4/cygnus-1.96/include/stddef.h" 1
//# 94 "/giga/hgs/lib/gcc/sun4/cygnus-1.96/include/stddef.h"

//# 3 "/giga/hgs/lib/g++-include/stddef.h" 2

}
//# 73 "/giga/hgs/lib/g++-include/stddef.h"

//# 6 "/giga/hgs/lib/g++-include/time.h" 2

//# 1 "/giga/hgs/lib/g++-include/stdio.h" 1
















































//#pragma interface



















//# 80 "/giga/hgs/lib/g++-include/stdio.h"



//# 117 "/giga/hgs/lib/g++-include/stdio.h"





//# 153 "/giga/hgs/lib/g++-include/stdio.h"



extern "C" {




























//# 1 "/usr/include/stdio.h" 1





extern	struct	_iobuf {
	int	_cnt;
	unsigned char *_ptr;
	unsigned char *_base;
	int	_bufsiz;
	short	_flag;
	char	_file;
} _iob[];







































extern struct _iobuf 	*c_proto_fopen ();
extern struct _iobuf 	*c_proto_fdopen ();
extern struct _iobuf 	*c_proto_freopen ();
extern struct _iobuf 	*c_proto_popen ();
extern struct _iobuf 	*tmpfile();
extern long	ftell();
extern char	*fgets();
extern char	*gets();
extern char	*c_proto_sprintf ();
extern char	*ctermid();
extern char	*cuserid();
extern char	*c_proto_tempnam ();
extern char	*tmpnam();






//# 185 "/giga/hgs/lib/g++-include/stdio.h" 2































}
//# 417 "/giga/hgs/lib/g++-include/stdio.h"






extern "C" {







int    _doprnt(const char*, void*, struct _iobuf *);
int    _doscan(struct _iobuf *, const char*, ...);
int    _filbuf(struct _iobuf *);
int    _flsbuf(unsigned, struct _iobuf *);

int    fclose(struct _iobuf *);
struct _iobuf *  fdopen(int, const char*);
int    fflush(struct _iobuf *);
int    fgetc(struct _iobuf *);
char*  fgets(char*, int, struct _iobuf  *);
struct _iobuf *  fopen(const char*, const char*);
int    fprintf(struct _iobuf *, const char* ...);
int    fputc(int, struct _iobuf *);
int    fputs(const char*, struct _iobuf *);
int fread(void*, int, int, struct _iobuf *);



struct _iobuf *  freopen(const char*, const char*, struct _iobuf *);

int    fscanf(struct _iobuf *, const char* ...);
int    fseek(struct _iobuf *, long, int);
long   ftell(struct _iobuf  *);
int fwrite(const void*, int, int, struct _iobuf *);
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
int    setbuf(struct _iobuf *, char*);
int    setbuffer(struct _iobuf *, char*, int);
int    setlinebuf(struct _iobuf *);
int    setvbuf(struct _iobuf *, char*, int, int);
int    sscanf(char*, const char* ...);
struct _iobuf *  tmpfile();
int    ungetc(int, struct _iobuf *);
int    vfprintf(struct _iobuf *, const char*, ...);




int    vprintf(const char*, ... );





int  sprintf(char*, const char*, ...);
char*  vsprintf(char*, const char*, ...);


}

















//# 7 "/giga/hgs/lib/g++-include/time.h" 2


//# 1 "/giga/hgs/lib/g++-include/sys/types.h" 1


//# 1 "/giga/hgs/lib/g++-include/stddef.h" 1

extern "C" {
//# 1 "/giga/hgs/lib/gcc/sun4/cygnus-1.96/include/stddef.h" 1
//# 94 "/giga/hgs/lib/gcc/sun4/cygnus-1.96/include/stddef.h"

//# 3 "/giga/hgs/lib/g++-include/stddef.h" 2

}
//# 73 "/giga/hgs/lib/g++-include/stddef.h"

//# 3 "/giga/hgs/lib/g++-include/sys/types.h" 2




extern "C"
{


























//# 1 "/usr/include/sys/types.h" 1








//# 115 "/usr/include/sys/types.h"

//# 35 "/giga/hgs/lib/g++-include/sys/types.h" 2

































}




//# 9 "/giga/hgs/lib/g++-include/time.h" 2


extern "C" {

















//# 42 "/giga/hgs/lib/g++-include/time.h"







//# 1 "/usr/include/time.h" 1





//# 1 "/usr/include/sys/stdtypes.h" 1










//# 32 "/usr/include/sys/stdtypes.h"

//# 6 "/usr/include/time.h" 2




struct	tm {
	int	tm_sec;
	int	tm_min;
	int	tm_hour;
	int	tm_mday;
	int	tm_mon;
	int	tm_year;
	int	tm_wday;
	int	tm_yday;
	int	tm_isdst;
	char	*tm_zone;
	long	tm_gmtoff;
};

extern	struct tm *c_proto_gmtime (), *c_proto_localtime ();
extern	char *c_proto_asctime (), *c_proto_ctime ();
extern	void c_proto_tzset (), c_proto_tzsetwall ();
extern  int dysize();
extern  time_t timelocal(), timegm();


//# 49 "/giga/hgs/lib/g++-include/time.h" 2


//# 1 "/usr/include/sys/times.h" 1









//# 1 "/giga/hgs/lib/g++-include/sys/types.h" 1


//# 1 "/giga/hgs/lib/g++-include/stddef.h" 1

extern "C" {
//# 1 "/giga/hgs/lib/gcc/sun4/cygnus-1.96/include/stddef.h" 1
//# 94 "/giga/hgs/lib/gcc/sun4/cygnus-1.96/include/stddef.h"

//# 3 "/giga/hgs/lib/g++-include/stddef.h" 2

}
//# 73 "/giga/hgs/lib/g++-include/stddef.h"

//# 3 "/giga/hgs/lib/g++-include/sys/types.h" 2




extern "C"
{


























//# 1 "/usr/include/sys/types.h" 1








//# 115 "/usr/include/sys/types.h"

//# 35 "/giga/hgs/lib/g++-include/sys/types.h" 2

































}




//# 10 "/usr/include/sys/times.h" 2


struct tms {
	clock_t	tms_utime;
	clock_t	tms_stime;
	clock_t	tms_cutime;
	clock_t	tms_cstime;
};


clock_t times( );



//# 51 "/giga/hgs/lib/g++-include/time.h" 2





















extern struct tm* localtime(long*);
extern struct tm* gmtime(long*);
extern char* ctime(long*);
extern char* asctime(struct tm*);
extern void tzset();
extern void tzsetwall();






extern long times(struct tms*);


//# 97 "/giga/hgs/lib/g++-include/time.h"

extern char* timezone(int, int);
extern int getitimer(int, struct itimerval*);
extern int setitimer(int, struct itimerval*, struct itimerval*);
extern int gettimeofday(struct timeval*, struct timezone*);
extern int settimeofday(struct timeval*, struct timezone*);
extern int stime(long*);
int       dysize(int);








long      clock(void);

long      time(long*);
unsigned  ualarm(unsigned, unsigned);
unsigned  usleep(unsigned);
int       profil(char*, int, int, int);

}



//# 1 "/giga/hgs/lib/g++-include/sys/times.h" 2

//# 311 "../../../../libg++/etc/benchmarks/dhrystone.cc" 2
























typedef enum	{Ident1, Ident2, Ident3, Ident4, Ident5} Enumeration;




typedef Int	OneToThirty;
typedef Int	OneToFifty;
typedef Char	CapitalLetter;
typedef Char	String30[31];
typedef Int	Array1Dim[51];
typedef Int	Array2Dim[51][51];

struct	Record
{
	struct Record		*PtrComp;
	Enumeration		Discr;
	Enumeration		EnumComp;
	OneToFifty		IntComp;
	String30		StringComp;
};

typedef struct Record 	RecordType;
typedef RecordType *	RecordPtr;
typedef int		boolean;











extern "C" {
extern int printf(const char* ...);
extern void exit(int);
}

void Proc0();
void Proc1(RecordPtr PtrParIn);
void Proc2(OneToFifty	*IntParIO);
void Proc3(RecordPtr	*PtrParOut);
void Proc4();
void Proc5();
boolean Func3(Enumeration	EnumParIn);
void Proc6(  Enumeration	EnumParIn,   Enumeration	*EnumParOut);
void Proc7(OneToFifty IntParI1, OneToFifty IntParI2, OneToFifty *IntParOut);
void Proc8(Array1Dim	Array1Par,
      Array2Dim	Array2Par,
      OneToFifty IntParI1,
      OneToFifty IntParI2);
Enumeration Func1(CapitalLetter	CharPar1, CapitalLetter	CharPar2);
boolean Func2(String30	StrParI1, String30	StrParI2);
boolean Func3(Enumeration	EnumParIn);

void mystrcpy(String30 s, char* t)
{
  for (; *t != '\0'; ++s, ++t) *s = *t;
  *s = '\0';
}

char mystrcmp(String30 s, String30 t)
{
  for (; *s == *t; ++s, ++t) if (*s == '\0') return 0;
  return char(*s - *t);
}



main()
{
	Proc0();
	exit(0);
}




Int		IntGlob;
boolean		BoolGlob;
char		Char1Glob;
char		Char2Glob;
Array1Dim	Array1Glob;
Array2Dim	Array2Glob;
RecordPtr	PtrGlb;
RecordPtr	PtrGlbNext;

void Proc0()
{
	OneToFifty		IntLoc1;
	  OneToFifty		IntLoc2;
	OneToFifty		IntLoc3;
	  char		CharLoc;
	  char		CharIndex;
	Enumeration	 	EnumLoc;
	String30		String1Loc;
	String30		String2Loc;

//# 445 "../../../../libg++/etc/benchmarks/dhrystone.cc"


	time_t			starttime;
	time_t			benchtime;
	time_t			nulltime;
	struct tms		Tms;
	register unsigned int	i;

	times(&Tms); starttime = Tms.tms_utime;
	for (i = 0; i < 500000		; ++i);
	times(&Tms);
	nulltime = Tms.tms_utime - starttime;


	PtrGlbNext = new Record;
	PtrGlb = new Record;
	PtrGlb->PtrComp = PtrGlbNext;
	PtrGlb->Discr = Ident1;
	PtrGlb->EnumComp = Ident3;
	PtrGlb->IntComp = 40;
	mystrcpy(PtrGlb->StringComp, "DHRYSTONE PROGRAM, SOME STRING");
	mystrcpy(String1Loc, "JUST INITIALIZED TO SOME JUNK.");








	times(&Tms); starttime = Tms.tms_utime;

	for (i = 0; i < 500000		; ++i)
	{

		Proc5();
		Proc4();
		IntLoc1 = 2;
		IntLoc2 = 3;
		mystrcpy(String2Loc, "DHRYSTONE PROGRAM, 2'ND STRING");
		EnumLoc = Ident2;
		BoolGlob = ! Func2(String1Loc, String2Loc);
		while (IntLoc1 < IntLoc2)
		{
			IntLoc3 = 5 * IntLoc1 - IntLoc2;
			Proc7(IntLoc1, IntLoc2, &IntLoc3);
			++IntLoc1;
		}
		Proc8(Array1Glob, Array2Glob, IntLoc1, IntLoc3);
		Proc1(PtrGlb);
		for (CharIndex = 'A'; CharIndex <= Char2Glob; ++CharIndex)
			if (EnumLoc == Func1(CharIndex, 'C'))
				Proc6(Ident1, &EnumLoc);
		IntLoc3 = IntLoc2 * IntLoc1;
		IntLoc2 = IntLoc3 / IntLoc1;
		IntLoc2 = 7 * (IntLoc3 - IntLoc2) - IntLoc1;
		Proc2(&IntLoc1);
	}













	times(&Tms);
	benchtime = Tms.tms_utime - starttime - nulltime;
	printf("Dhrystone time for %ld passes = %ld\n",
		(long) 500000		, benchtime/60		);
	printf("This machine benchmarks at %ld dhrystones/second\n",
		((long) 500000		) * 60		 / benchtime);


}

void Proc1(RecordPtr PtrParIn)
{


	(*(PtrParIn->PtrComp))  =  *PtrGlb ;
	PtrParIn->IntComp = 5;
	(*(PtrParIn->PtrComp)) .IntComp = PtrParIn->IntComp;
	(*(PtrParIn->PtrComp)) .PtrComp = PtrParIn->PtrComp;

	Proc3(&((*(PtrParIn->PtrComp)) .PtrComp));
	if ((*(PtrParIn->PtrComp)) .Discr == Ident1)
	{
		(*(PtrParIn->PtrComp)) .IntComp = 6;
		Proc6(PtrParIn->EnumComp, &(*(PtrParIn->PtrComp)) .EnumComp);
		(*(PtrParIn->PtrComp)) .PtrComp = PtrGlb->PtrComp;
		Proc7((*(PtrParIn->PtrComp)) .IntComp, 10, &(*(PtrParIn->PtrComp)) .IntComp);
	}
	else
		*PtrParIn =  (*(PtrParIn->PtrComp))  ;


}

void Proc2(OneToFifty	*IntParIO)
{
	  OneToFifty		IntLoc;
	  Enumeration		EnumLoc;

	IntLoc = *IntParIO + 10;
	for(;;)
	{
		if (Char1Glob == 'A')
		{
			--IntLoc;
			*IntParIO = IntLoc - IntGlob;
			EnumLoc = Ident1;
		}
		if (EnumLoc == Ident1)
			break;
	}
}

void Proc3(RecordPtr	*PtrParOut)
{
	if (PtrGlb != 0 )
		*PtrParOut = PtrGlb->PtrComp;
	else
		IntGlob = 100;
	Proc7(10, IntGlob, &PtrGlb->IntComp);
}

void Proc4()
{
	  boolean	BoolLoc;

	BoolLoc = Char1Glob == 'A';
	BoolLoc |= BoolGlob;
	Char2Glob = 'B';
}

void Proc5()
{
	Char1Glob = 'A';
	BoolGlob = 	0 ;
}




void Proc6(  Enumeration	EnumParIn,   Enumeration	*EnumParOut)
{
	*EnumParOut = EnumParIn;
	if (! Func3(EnumParIn) )
		*EnumParOut = Ident4;
	switch (EnumParIn)
	{
	case Ident1:	*EnumParOut = Ident1; break;
	case Ident2:	if (IntGlob > 100) *EnumParOut = Ident1;
			else *EnumParOut = Ident4;
			break;
	case Ident3:	*EnumParOut = Ident2; break;
	case Ident4:	break;
	case Ident5:	*EnumParOut = Ident3;
	}
}

void Proc7(OneToFifty IntParI1, OneToFifty IntParI2, OneToFifty *IntParOut)
{
	  OneToFifty	IntLoc;

	IntLoc = IntParI1 + 2;
	*IntParOut = IntParI2 + IntLoc;
}

void Proc8(Array1Dim	Array1Par,
      Array2Dim	Array2Par,
      OneToFifty IntParI1,
      OneToFifty IntParI2)
{
	  OneToFifty	IntLoc;
	  OneToFifty	IntIndex;

	IntLoc = IntParI1 + 5;
	Array1Par[IntLoc] = IntParI2;
	Array1Par[IntLoc+1] = Array1Par[IntLoc];
	Array1Par[IntLoc+30] = IntLoc;
	for (IntIndex = IntLoc; IntIndex <= (IntLoc+1); ++IntIndex)
		Array2Par[IntLoc][IntIndex] = IntLoc;
	++Array2Par[IntLoc][IntLoc-1];
	Array2Par[IntLoc+20][IntLoc] = Array1Par[IntLoc];
	IntGlob = 5;
}

Enumeration Func1(CapitalLetter	CharPar1, CapitalLetter	CharPar2)
{
	  CapitalLetter	CharLoc1;
	  CapitalLetter	CharLoc2;

	CharLoc1 = CharPar1;
	CharLoc2 = CharLoc1;
	if (CharLoc2 != CharPar2)
		return (Ident1);
	else
		return (Ident2);
}

boolean Func2(String30	StrParI1, String30	StrParI2)
{
	  OneToThirty		IntLoc;
	  CapitalLetter	CharLoc;

	IntLoc = 1;
	while (IntLoc <= 1)
		if (Func1(StrParI1[IntLoc], StrParI2[IntLoc+1]) == Ident1)
		{
			CharLoc = 'A';
			++IntLoc;
		}
	if (CharLoc >= 'W' && CharLoc <= 'Z')
		IntLoc = 7;
	if (CharLoc == 'X')
		return(	1 );
	else
	{
		if (mystrcmp(StrParI1, StrParI2) > 0)
		{
			IntLoc += 7;
			return (	1 );
		}
		else
			return (	0 );
	}
}

boolean Func3(Enumeration	EnumParIn)
{
	  Enumeration	EnumLoc;

	EnumLoc = EnumParIn;
	if (EnumLoc == Ident3) return (	1 );
	return (	0 );
}
