/* { dg-do compile } */
/* { dg-require-visibility "" } */

typedef unsigned int size_t;
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Iterator > struct iterator_traits
  {
  };
  template < typename _Tp > struct iterator_traits <_Tp * >
  {
    typedef _Tp & reference;
  };
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  using std::iterator_traits;
  template < typename _Iterator, typename _Container > class __normal_iterator
  {
  public:typedef _Iterator iterator_type;
    typedef typename iterator_traits < _Iterator >::reference reference;
    reference operator* () const
    {
    }
    __normal_iterator operator++ (int)
    {
    }
  };
  template < typename _IteratorL, typename _IteratorR,
    typename _Container > inline bool operator!= (const __normal_iterator <
						  _IteratorL,
						  _Container > &__lhs,
						  const __normal_iterator <
						  _IteratorR,
						  _Container > &__rhs)
  {
  }
}

extern "C"
{
  extern "C"
  {
    __extension__ typedef __SIZE_TYPE__ __intptr_t;
  }
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  template < typename _Tp > class new_allocator
  {
  public:typedef size_t size_type;
    typedef _Tp *pointer;
    template < typename _Tp1 > struct rebind
    {
      typedef new_allocator < _Tp1 > other;
    };
  };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
template < typename _Tp > class allocator:public __gnu_cxx::new_allocator <
    _Tp >
  {
  };
}

extern "C"
{
  typedef __intptr_t intptr_t;
}
namespace llvm
{
  template < typename NodeTy > class ilist_half_node
  {
  };
template < typename NodeTy > class ilist_node:private ilist_half_node <
    NodeTy >
  {
  };
  class MachineBasicBlock;
  class MachineOperand
  {
  public:enum MachineOperandType
    {
    }
    Contents;
    unsigned getReg () const
    {
    }
  };
  class TargetRegisterInfo;
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Tp, typename _Alloc > struct _Vector_base
  {
    typedef typename _Alloc::template rebind < _Tp >::other _Tp_alloc_type;
  };
template < typename _Tp, typename _Alloc = std::allocator < _Tp > >class vector:protected _Vector_base < _Tp,
    _Alloc
    >
  {
    typedef _Vector_base < _Tp, _Alloc > _Base;
    typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
  public:typedef _Tp value_type;
    typedef typename _Tp_alloc_type::pointer pointer;
    typedef __gnu_cxx::__normal_iterator < pointer, vector > iterator;
    iterator begin ()
    {
    }
    iterator end ()
    {
    }
  };
}

namespace llvm
{
  class MachineFunction;
  class MachineInstr:public ilist_node < MachineInstr >
  {
  public:const MachineBasicBlock *getParent () const
    {
    }
    const MachineOperand & getOperand (unsigned i) const
    {
    }
    bool registerDefIsDead (unsigned Reg, const TargetRegisterInfo * TRI =
			    __null) const
    {
    }
  };
  class AnalysisResolver;
  class Pass
  {
    AnalysisResolver *Resolver;
    intptr_t PassID;
  public:  explicit Pass (intptr_t pid):Resolver (0), PassID (pid)
    {
    }
    explicit Pass (const void *pid):Resolver (0), PassID ((intptr_t) pid)
    {
    }
    template < typename AnalysisType > AnalysisType & getAnalysis () const;
  };
  class FunctionPass:public Pass
  {
  public:explicit FunctionPass (intptr_t pid):Pass (pid)
    {
    }
    explicit FunctionPass (const void *pid):Pass (pid)
    {
    }
  };
  class PassInfo
  {
  public:typedef Pass *(*NormalCtor_t) ();
  private:const char *const PassName;
    const char *const PassArgument;
    const intptr_t PassID;
    const bool IsCFGOnlyPass;
    const bool IsAnalysis;
    const bool IsAnalysisGroup;
    NormalCtor_t NormalCtor;
  public:   PassInfo (const char *name, const char *arg, intptr_t pi, NormalCtor_t normal = 0, bool isCFGOnly = false, bool is_analysis = false):PassName (name), PassArgument (arg), PassID (pi),
      IsCFGOnlyPass (isCFGOnly), IsAnalysis (is_analysis),
      IsAnalysisGroup (false), NormalCtor (normal)
    {
    }
  };
  template < typename PassName > Pass * callDefaultCtor ()
  {
    return new PassName ();
  }
  template < typename passName > struct RegisterPass:public PassInfo
  {
  RegisterPass (const char *PassArg, const char *Name, bool CFGOnly = false, bool is_analysis = false):PassInfo (Name, PassArg, intptr_t (&passName::ID),
	      PassInfo::NormalCtor_t (callDefaultCtor < passName >), CFGOnly,
	      is_analysis)
    {
    }
  };
  template < typename T > class SmallVectorImpl
  {
  };
  template < typename T,
    unsigned N > class SmallVector:public SmallVectorImpl < T >
  {
  };
  class MachineFunctionPass:public FunctionPass
  {
  protected:explicit MachineFunctionPass (intptr_t ID):FunctionPass (ID)
    {
    }
    explicit MachineFunctionPass (void *ID):FunctionPass (ID)
    {
    }
    virtual bool runOnMachineFunction (MachineFunction & MF) = 0;
  };
  class LiveIndex
  {
  private:unsigned index;
  };
  class VNInfo
  {
  };
  struct LiveRange
  {
    LiveIndex start;
    LiveIndex end;
    VNInfo *valno;
  };
  class LiveInterval
  {
  public:typedef SmallVector < LiveRange, 4 > Ranges;
    bool containsOneValue () const
    {
    }
    LiveRange *getLiveRangeContaining (LiveIndex Idx)
    {
    }
    void removeRange (LiveIndex Start, LiveIndex End, bool RemoveDeadValNo =
		      false);
    void removeRange (LiveRange LR, bool RemoveDeadValNo = false)
    {
      removeRange (LR.start, LR.end, RemoveDeadValNo);
    }
  };
  class LiveIntervals:public MachineFunctionPass
  {
  public:static char ID;
    LiveIndex getDefIndex (LiveIndex index)
    {
    }
    LiveInterval & getInterval (unsigned reg)
    {
    }
    LiveIndex getInstructionIndex (const MachineInstr * instr) const
    {
    }
  };
}

using namespace llvm;
namespace
{
struct __attribute__ ((visibility ("hidden"))) StrongPHIElimination:public
    MachineFunctionPass
  {
    static char ID;
  StrongPHIElimination ():MachineFunctionPass (&ID)
    {
    }
    bool runOnMachineFunction (MachineFunction & Fn);
  };
}

static RegisterPass < StrongPHIElimination > X ("strong-phi-node-elimination",
						"Eliminate PHI nodes for register allocation, intelligently");
bool
StrongPHIElimination::runOnMachineFunction (MachineFunction & Fn)
{
  LiveIntervals & LI = getAnalysis < LiveIntervals > ();
  std::vector < MachineInstr * >phis;
  for (std::vector < MachineInstr * >::iterator I = phis.begin (), E =
       phis.end (); I != E;)
    {
      MachineInstr *PInstr = *(I++);
      unsigned DestReg = PInstr->getOperand (0).getReg ();
      LiveInterval & PI = LI.getInterval (DestReg);
      if (PInstr->registerDefIsDead (DestReg))
	{
	  if (PI.containsOneValue ())
	    {
	      LiveIndex idx =
		LI.getDefIndex (LI.getInstructionIndex (PInstr));
	      PI.removeRange (*PI.getLiveRangeContaining (idx), true);
	    }
	}
    }
}
