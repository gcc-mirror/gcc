// { dg-lto-do assemble }
// { dg-lto-options {{-g -flto}} }

namespace llvm
{
  class Function;
  class MachineCodeInfo;
  class ExecutionEngine
    {
    };
  class JIT : public ExecutionEngine
  {
    void runJITOnFunction (Function * F, MachineCodeInfo * MCI = 0);
  };
  class JITEventListener
    {
    public:
      JITEventListener ()
	{
	}
      virtual ~JITEventListener ();
    };
}

using namespace llvm;
void
JIT::runJITOnFunction (Function * F, MachineCodeInfo * MCI)
{
  class MCIListener:public JITEventListener
  {
    MachineCodeInfo *const MCI;
  public: 
    MCIListener (MachineCodeInfo * mci):MCI (mci)
	 {
	 }
  };
}

