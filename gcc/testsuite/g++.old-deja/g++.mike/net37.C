// { dg-do run  }
class B {
public:
    int bi;
    void bProc ( void ) { bi = 39; }
  };

class D : public B {
public:
    int di;
    void dProc (void ){ di = 42; }
  };

typedef void (B::*BPROC)(void);
typedef void (D::*DPROC)(void);

union AFX_PMSG {
public:

  AFX_PMSG () {};
  AFX_PMSG ( BPROC bpr ) { bfn = bpr ; }

  operator BPROC() { return bfn; }

  BPROC bfn;
  DPROC dfn;
};


int main(int argc, char *argv[]) {
  B b;
  D d;

  BPROC    bpr = &B::bProc;
  AFX_PMSG pmsg(bpr);

  BPROC ppr = pmsg;

  return 0;
}
