class XclRoot { // { dg-lto-warning "7: type 'struct XclRoot' violates the C\\+\\+ One Definition Rule" }
public:
  virtual ~XclRoot();
};
class XclImpRoot : XclRoot {};
struct RootData {
  XclImpRoot pIR;
};
class ExcRoot {
RootData pExcRoot;
};
class XclImpAutoFilterData : ExcRoot {
  void SetExtractPos(const int &);
};
void XclImpAutoFilterData::SetExtractPos(const int &) {}
