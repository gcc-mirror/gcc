class XclRoot { // { dg-lto-warning "7: type 'struct XclRoot' violates the C\\+\\+ One Definition Rule" }
public:
  virtual ~XclRoot();
};
class XclImpRoot : XclRoot {};
struct RootData { // { dg-lto-warning "8: type 'struct RootData' violates the C\\+\\+ One Definition Rule" }
  XclImpRoot pIR;
};
class ExcRoot {
RootData pExcRoot;
};
class XclImpAutoFilterData : ExcRoot {
  void SetExtractPos(const int &);
};
void XclImpAutoFilterData::SetExtractPos(const int &) {}
