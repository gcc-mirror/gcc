// { dg-do assemble  }
// Origin: rch@larissa.sd.bi.ruhr-uni-bochum.de

template< class X >
struct VB: public virtual X
{};

template< class MOPTerm1, class MOPTerm2 >
struct MOPTermUnify
{
  struct MO:
    public VB<MOPTerm1>,
    public VB<MOPTerm2>
  {
    void   fix()
      { 
      }
  };
};
