// Bug: Segfaults on egcs 1.0.1 i586-pc-linux-glibc1
// From: Max Lawson <mlawson@drfmc.ceng.cea.fr>
// Message-Id: <9803091022.AA07520@drfmc.ceng.cea.fr>

class S0
{
public:

  S0() { };

  virtual ~S0() { }
};



struct S { };
class S1 : public S, public S0
{ 
public:

  S1() { }
};


void test_ptr(void *ctxt)
{
  S0 *ctxt1 = static_cast<S0*>(ctxt);

  S1* ctxt2 = dynamic_cast<S1*>(ctxt1);
}


int main()
{
  S1 *ctxt = new S1();

  test_ptr(ctxt);

  return 0;
}


