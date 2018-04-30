class XclRoot {
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
