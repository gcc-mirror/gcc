// Class definitions for dllexport-MI1.C and dllimport-MI1.C

#ifdef BUILDING_MI_DLL
#define  DLL_IMPEXP __attribute__ ((dllexport))
#else
#define  DLL_IMPEXP __attribute__ ((dllimport))
#endif


#define D1_return 1
#define D2_return 2

class DLL_IMPEXP MBase
{
public:
  virtual int vf() const = 0;
  virtual ~MBase();
};

class DLL_IMPEXP D1 : virtual public MBase
{
public:
  int vf() const;
};

class DLL_IMPEXP D2 : virtual public MBase
{
public:
  D2 ();
  D2 (D2 const&);
  int vf() const;
};

class DLL_IMPEXP MI1 : public D1, public D2
{
public:
  int vf() const;
};

