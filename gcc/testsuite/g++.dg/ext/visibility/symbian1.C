// { dg-do compile { target arm*-*-symbianelf* } }
// Class data should not be exported.
// { dg-final { scan-hidden "_ZTV2K3" } }
// But the constructor and destructor should be exported.
// { dg-final { scan-not-hidden "_ZN2K3C2Ev" } }
// { dg-final { scan-not-hidden "_ZN2K3D0Ev" } }

class __declspec(notshared) K3 {
public:
  __declspec(dllimport) K3();
  __declspec(dllimport) virtual ~K3();
  virtual int m1();
};

__declspec(dllexport)
  K3::K3(){}

__declspec(dllexport)
  K3::~K3(){}

int K3::m1() { return 1; }

