// { dg-do compile }
// { dg-options "-ftree-vrp -fno-guess-branch-probability -fnon-call-exceptions" }
// { dg-additional-options "-Wno-return-type" }

void *xalloc ();
void xfree (void *);
void error ();

static inline void *
MallocT ()
{
  void *p = xalloc ();
  if (!p)
    error ();
  return p;
}


struct ByteBlob
{
  int *header;

  ByteBlob();

  ~ByteBlob ()
  {
    Free ();
  }

  int RawFree (int * p)
  {
    if (!p)
      error ();
    xfree (p);
  }

  int *LengthRef ();

  void Free ()
  {
    if (*header)
      RawFree (header);
  }

  int Append (int num_ints)
  {
    if (*header)
      MallocT ();
    *LengthRef () += num_ints;
  }
};

struct CBlobT:ByteBlob
{
  ~CBlobT ()
  {
    Free ();
  }
};

template < class T > struct FixedSizeArray
{
  int HeaderSize;
  T *data;
  FixedSizeArray ();
  int RefCnt ()
  {
    return *(int *) MallocT ();
  }
   ~FixedSizeArray ()
  {
    if (RefCnt ())
      for (T * pItem = data + Length (); pItem != data; pItem--)
	T ();
  }
  int Length ();
};

class SmallArray
{
  typedef FixedSizeArray < int > SubArray;
  typedef FixedSizeArray < SubArray > SuperArray;
  SuperArray data;
};

struct CHashTableT
{
  int *m_slots;
  ~CHashTableT ()
  {
    delete m_slots;
  }
};

struct CYapfBaseT
{
  int *PfGetSettings ();
  SmallArray m_arr;
  CHashTableT m_closed;
  CYapfBaseT ()
  {
    MallocT ();
  }
};

struct CYapfCostRailT:CYapfBaseT
{
  CBlobT m_sig_look_ahead_costs;
  CYapfCostRailT ()
  {
    m_sig_look_ahead_costs.Append (*Yapf ()->PfGetSettings ());
    Yapf ()->PfGetSettings ();
  }
  CYapfBaseT *Yapf ();
};

void stCheckReverseTrain ()
{
  CYapfCostRailT pf1;
}
