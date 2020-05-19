// { dg-do assemble  }
// { dg-options "-O2 -g" }
// (Should preferrably cycle through options.)
// Origin: Hans-Peter Nilsson <hp@axis.com>
// See <URL:http://gcc.gnu.org/ml/gcc-patches/2000-06/msg00310.html>

typedef unsigned udword __attribute__((mode (__SI__)));
extern "C" {
void __assert (const char *, int, const char *);
}
class ShZzy;
class ShBe;
class Sh
{
 public:
  class Xy;
  inline  Sh(const char*      theName,
               const Xy& theXys);
  virtual ~Sh();
  inline const char* name() const;
  inline bool        shXy(const Xy& theXy);
  virtual void       setParent(Sh* theParent);
  class Xy
  {
   public:
    Xy(int   theXy);
    Xy(const Xy& theXy);
    Xy& operator = (const Xy&);
    bool sh(const Xy& theXy);
   private:
    int  myXyFlags;
  };
 protected:
  Xy     myXys;
 private:
  Sh();
  Sh(const Sh&);
  Sh& operator = (const Sh&);
  const char*   myName;
  Sh*         myParent;
};
class ShZzy : public Sh 
{
 public:
  inline ShZzy(const char* theName);
  inline ShZzy(const char* theName,
                   const Xy& theXys);
  virtual ~ShZzy();
  void         addShTo(char* theParent);
  virtual void ap() {}
  void         addSh(Sh* theSh);
  virtual void setParent(Sh* theParent);
  void         baseAp();
 private:
  ShZzy();
  ShZzy(const ShZzy&);
  ShZzy& operator = (const ShZzy&);
  bool          iAmInited;
};
class ShBop
{
 public:
  inline ShBop(const ShBe* theBe);
  void            shWw(bool,
                                const char* a1 = "",
                                const char* a2 = "",
                                int   a3 = -1,
                                const char* a4 = "foo");
  typedef enum { shOk,
                 shFailed,
                 shDone,
                 invalidBop } bopType;
  inline bopType bop();
  inline int        numberOfVs();
  inline void       closeBop();
 private:
  ShBop(const ShBop&);
  ShBop& operator = (const ShBop&);
  const ShBe*  myBe;
  bopType       myBop;
  int              my1;
  int              my2;
  const char*      my3;
  const char*      my4;
  int              my5;
  const char*      my6;
};
Sh::Sh(const char* theName,
           const Xy& theXys)
  :myXys(theXys),
   myName(theName),
   myParent(0)
{}
const char*
Sh::name() const
{
  return myName;
}
bool
Sh::shXy(const Xy& theXy)
{
  return myXys.sh(theXy);
}
ShZzy::ShZzy(const char* theName)
  :Sh(theName, Xy(0)),
   iAmInited(false)
{}
ShZzy::ShZzy(const char* theName,
                     const Xy& theXys)
  :Sh(theName, theXys),
   iAmInited(false)
{}
ShBop::ShBop(const ShBe* theBe)
  :myBe(theBe),
   myBop(invalidBop),
   my1(0),
   my2(0),
   my3(""),
   my4(""),
   my5(0),
   my6("")
{}
ShBop::bopType
ShBop::bop()
{
  ((!my2 || myBop == shFailed) ? (void)0 : __assert("foo", 91,"foo"));
  return myBop;
}
int
ShBop::numberOfVs()
{
  return my2 + my1;
}
void
ShBop::closeBop()
{
  if (myBop == invalidBop)
  {
    myBop = shDone;
  }
}
class ShBe;
template <class ShCc>
class ShAp : public ShZzy
{
 public:
  ShAp(const char* theName);
  ShAp(const char* theName,
                const Xy& theXys);
  virtual ~ShAp() {}
  typedef void (ShCc::*ShMethod)();
  virtual void ap() {}
 protected:
  ShBop* bop();
 private:
  ShAp();
  ShAp(const ShAp&);
  ShAp& operator = (const ShAp&);
  ShBop* myBop;
  ShBe*   myBe;
};
class ShBe : public Sh
{
 public:
  inline ShBe(const char* theName);
  inline ShBe(const char* theName,
                  const Xy& theXys);
  inline virtual ~ShBe();
  virtual void    run() = 0;
  ShBop*  runBe();
 protected:
  inline ShBop* bop();
 private:
  ShBe();
  ShBe(const ShBe&);
  ShBe& operator = (const ShBe&);
  ShBop* myBop;
};
template <class ShCc> 
ShAp<ShCc>::ShAp(const char* theName)
  : ShZzy(theName),
    myBop(0),
    myBe(0)
{}
template <class ShCc> 
ShAp<ShCc>::ShAp(const char* theName,
                                   const Sh::Xy& theXys)
  : ShZzy(theName, theXys),
    myBop(0),
    myBe(0)
{}
template <class ShCc>
ShBop*
ShAp<ShCc>::bop()
{
  ((myBop) ? (void)0 : __assert("foo", 96,"foo"));
  return myBop;
}
class xSh : public ShAp<xSh>
{
 public:
  xSh();
  virtual ~xSh();
  void ap();
  void uff();
  void rtt();
};
class xUff
{
 public:
  xUff();
  xUff(const xUff&);
  xUff& operator = (const xUff&);
  xUff(udword);
  operator udword() const;
  xUff& operator = (udword);
  bool operator < (const xUff) const;
  bool operator <= (const xUff) const;
  bool operator > (const xUff) const;
  bool operator >= (const xUff) const;
  bool operator == (const xUff) const;
  enum {size = 4};
  xUff(unsigned char* theUff);
 private:
  udword myUff;
};
inline
xUff::xUff()
  : myUff(0)
{
}
inline
xUff::xUff(udword theUff)
  : myUff(theUff)
{
}
inline
xUff::xUff(
  const xUff& theUff)
  : myUff(theUff.myUff)
{
}
inline xUff&
xUff::operator = (const xUff& theUff)
{
  myUff = theUff.myUff;
  return *this;
}
inline xUff&
xUff::operator = (udword theUff)
{
  myUff = theUff;
  return *this; 
}
inline
xUff::operator udword() const
{
  return myUff;
}
inline bool
xUff::operator < (const xUff ding) const
{
  return (((int) (myUff - ding.myUff)) < 0);
}
inline bool
xUff::operator <= (const xUff ding) const
{
  return (((int) (myUff - ding.myUff)) <= 0);
}
inline bool
xUff::operator > (const xUff ding) const
{
  return (((int) (myUff - ding.myUff)) > 0);
}
inline bool
xUff::operator >= (const xUff ding) const
{
  return (((int) (myUff - ding.myUff)) >= 0);
}
inline bool
xUff::operator == (const xUff ding) const
{
  return (myUff == ding.myUff);
}
inline
xUff::xUff(unsigned char* theUff)
{
  myUff = *(udword *)theUff;
}
void
xSh::uff()
{
  static const udword halfudword = 0x80000000;
  xUff aGah((udword)0);
  udword diagonal = halfudword + (udword) aGah;
  xUff aGeh(diagonal - 1);
  xUff aGoh(diagonal + 1);
  (bop()->shWw ((aGah.operator <=(aGah)), ("foo"), ( ""), 118, "foo"));
  (bop()->shWw ((aGah.operator >=(aGah)), ("foo"), ( ""), 119, "foo"));
  (bop()->shWw ((!(aGah.operator <(aGah))), ("foo"), ( ""), 120, "foo"));
  (bop()->shWw ((!(aGah.operator >(aGah))), ("foo"), ( ""), 121, "foo"));
  (bop()->shWw ((aGah.operator <(aGeh)), ("foo"), ( ""), 124, "foo"));
  (bop()->shWw ((aGah.operator <=(aGeh)), ("foo"), ( ""), 125, "foo"));
  (bop()->shWw ((!(aGah.operator >(aGeh))), ("foo"), ( ""), 126, "foo"));
  (bop()->shWw ((!(aGah.operator >=(aGeh))), ("foo"), ( ""), 127, "foo"));
  (bop()->shWw ((aGeh.operator >(aGah)), ("foo"), ( ""), 130, "foo"));
  (bop()->shWw ((aGeh.operator >=(aGah)), ("foo"), ( ""), 131, "foo"));
  (bop()->shWw ((!(aGeh.operator <(aGah))), ("foo"), ( ""), 132, "foo"));
  (bop()->shWw ((!(aGeh.operator <=(aGah))), ("foo"), ( ""), 133, "foo"));
  (bop()->shWw ((aGeh.operator <(aGoh)), ("foo"), ( ""), 136, "foo"));
  (bop()->shWw ((aGeh.operator <=(aGoh)), ("foo"), ( ""), 137, "foo"));
  (bop()->shWw ((!(aGeh.operator >(aGoh))), ("foo"), ( ""), 138, "foo"));
  (bop()->shWw ((!(aGeh.operator >=(aGoh))), ("foo"), ( ""), 139, "foo"));
  (bop()->shWw ((aGoh.operator >(aGeh)), ("foo"), ( ""), 142, "foo"));
  (bop()->shWw ((aGoh.operator >=(aGeh)), ("foo"), ( ""), 143, "foo"));
  (bop()->shWw ((!(aGoh.operator <(aGeh))), ("foo"), ( ""), 144, "foo"));
  (bop()->shWw ((!(aGoh.operator <=(aGeh))), ("foo"), ( ""), 145, "foo"));
  (bop()->shWw ((aGah.operator >(aGoh)), ("foo"), ( ""), 152, "foo"));
  (bop()->shWw ((aGah.operator >=(aGoh)), ("foo"), ( ""), 153, "foo"));
  (bop()->shWw ((!(aGah.operator <(aGoh))), ("foo"), ( ""), 154, "foo"));
  (bop()->shWw ((!(aGah.operator <=(aGoh))), ("foo"), ( ""), 155, "foo"));
  (bop()->shWw ((aGoh.operator <(aGah)), ("foo"), ( ""), 158, "foo"));
  (bop()->shWw ((aGoh.operator <=(aGah)), ("foo"), ( ""), 159, "foo"));
  (bop()->shWw ((!(aGoh.operator >(aGah))), ("foo"), ( ""), 160, "foo"));
  (bop()->shWw ((!(aGoh.operator >=(aGah))), ("foo"), ( ""), 161, "foo"));
}
