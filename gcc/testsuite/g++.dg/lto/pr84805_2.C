template < typename a, a b > struct c { static constexpr a d = b; };
typedef c< bool, false > e;
struct f : e {};
struct t : f {};
template < typename > struct g : e {};
namespace __gnu_cxx {
enum _Lock_policy { h, i, j } const k = j;
}
namespace std {
using __gnu_cxx::_Lock_policy;
using __gnu_cxx::k;
template < _Lock_policy = k > class _Sp_counted_base;
template < typename, _Lock_policy = k > class __shared_ptr;
template < _Lock_policy > class __shared_count { _Sp_counted_base<> *_M_pi; };
template < typename a, _Lock_policy, bool = g< a >::d, bool = t::d >
class __shared_ptr_access {};
template < typename a, _Lock_policy l >
class __shared_ptr : __shared_ptr_access< a, l > {
  using m = a;
  m *_M_ptr;
  __shared_count< l > _M_refcount;
};
template < typename a > class n : __shared_ptr< a > {};
} namespace rtl {
class OString {
  struct o *pData;
};
} using rtl::OString;
namespace rtl {
class OUString {
  struct p *pData;
};
} using rtl::OUString;
struct q {
  unsigned short m_value;
};
namespace tools {
template < typename r > class SvRef { r *pObj; };
} class SvRefBase {
  unsigned nRefCount : 31;
  unsigned bNoDelete : 1;

protected:
  virtual ~SvRefBase();
};
class SotObject : virtual SvRefBase {
  unsigned short nOwnerLockCount;
  bool bInClose;
};
class ErrCode {
  unsigned m_value;
};
class SvStream;
class BaseStorage;
class SotStorage : virtual SotObject {
  BaseStorage *m_pOwnStg;
  SvStream *m_pStorStm;
  ErrCode m_nError;
  OUString m_aName;
  bool m_bIsRoot;
  bool m_bDelStm;
  OString m_aKey;
  int m_nVersion;
};
class ScDocument;
class ScAddress {
  int nRow;
  short nCol;
  short nTab;
};
enum XclBiff {};
enum XclOutput {};
class SfxMedium;
class ScEditEngineDefaulter;
class ScHeaderEditEngine;
class EditEngine;
class ScExtDocOptions;
class XclFontPropSetHelper;
class XclChPropSetHelper;
class XclTracer;
struct RootData;
struct XclRootData {
  XclBiff meBiff;
  XclOutput meOutput;
  SfxMedium &mrMedium;
  tools::SvRef< SotStorage > mxRootStrg;
  ScDocument &mrDoc;
  OUString maDocUrl;
  OUString maBasePath;
  OUString maUserName;
  OUString maDefPassword;
  unsigned short meTextEnc;
  q meSysLang;
  q meDocLang;
  q meUILang;
  short mnDefApiScript;
  ScAddress maScMaxPos;
  ScAddress maXclMaxPos;
  ScAddress maMaxPos;
  std::n< ScEditEngineDefaulter > mxEditEngine;
  std::n< ScHeaderEditEngine > mxHFEditEngine;
  std::n< EditEngine > mxDrawEditEng;
  std::n< XclFontPropSetHelper > mxFontPropSetHlp;
  std::n< XclChPropSetHelper > mxChPropSetHlp;
  std::n< ScExtDocOptions > mxExtDocOpt;
  std::n< XclTracer > mxTracer;
  std::n< RootData > mxRD;
  virtual ~XclRootData();
};
class XclRoot {
  virtual ~XclRoot();
  XclRootData &mrData;
};
enum BiffTyp {};
class SharedFormulaBuffer;
class ExtNameBuff;
class ExtSheetBuffer;
class ExcelToSc;
class XclImpColRowSettings;
class XclImpRoot;
struct RootData {
  BiffTyp eDateiTyp;
  ExtSheetBuffer *pExtSheetBuff;
  SharedFormulaBuffer *pShrfmlaBuff;
  ExtNameBuff *pExtNameBuff;
  ExcelToSc *pFmlaConverter;
  XclImpColRowSettings *pColRowBuff;
  XclImpRoot *s;
};
XclRootData::~XclRootData() {}
XclRoot::~XclRoot() {}
