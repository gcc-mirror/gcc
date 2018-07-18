// { dg-lto-do link }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-O0 -fPIC -shared -flto}} }

template < typename _Tp, _Tp __v > struct integral_constant {
  static constexpr _Tp value = __v;
};
typedef integral_constant< bool, false > false_type;
struct __is_void_helper : false_type {};
struct is_void : __is_void_helper {};
template < typename > struct is_array : false_type {};
namespace __gnu_cxx {
enum _Lock_policy { _S_single, _S_mutex, _S_atomic }; // { dg-lto-warning "6: type '_Lock_policy' violates the C\\+\\+ One Definition Rule" }
const _Lock_policy __default_lock_policy = _S_atomic;
} namespace std {
using __gnu_cxx::_Lock_policy;
using __gnu_cxx::__default_lock_policy;
template < _Lock_policy = __default_lock_policy > class _Sp_counted_base;
template < typename, _Lock_policy = __default_lock_policy > class __shared_ptr;
template < _Lock_policy > class __shared_count { _Sp_counted_base<> *_M_pi; };
template < typename _Tp, _Lock_policy, bool = is_array< _Tp >::value,
           bool = is_void::value >
class __shared_ptr_access {};
template < typename _Tp, _Lock_policy _Lp >
class __shared_ptr : __shared_ptr_access< _Tp, _Lp > {
  using element_type = _Tp;
  element_type *_M_ptr;
  __shared_count< _Lp > _M_refcount;
};
template < typename _Tp > class shared_ptr : __shared_ptr< _Tp > {};
} typedef struct _rtl_String rtl_String;
typedef struct _rtl_uString rtl_uString;
namespace rtl {
class OString {
  rtl_String *pData;
};
} using rtl::OString;
namespace rtl {
class OUString {
  rtl_uString *pData;
};
} using rtl::OUString;
namespace tools {
template < typename T > class SvRef {
T *pObj;
};
} class SvRefBase {
  unsigned nRefCount : 31;
  unsigned bNoDelete : 1;

protected:
  virtual ~SvRefBase();
};
class ErrCode {
  unsigned m_value;
};
class SvStream;
class SfxMedium;
struct strong_int {
  unsigned short m_value;
};
typedef strong_int LanguageType;
class SotObject : virtual SvRefBase {
  unsigned short nOwnerLockCount;
  bool bInClose;
};
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
enum BiffTyp {};
class SharedFormulaBuffer;
class ExtNameBuff;
class ExtSheetBuffer;
class ExcelToSc;
class XclImpColRowSettings;
struct RootData { 
  BiffTyp eDateiTyp;
  ExtSheetBuffer *pExtSheetBuff;
  SharedFormulaBuffer *pShrfmlaBuff;
  ExtNameBuff *pExtNameBuff;
  ExcelToSc *pFmlaConverter;
  XclImpColRowSettings *pColRowBuff;
};
class ScEditEngineDefaulter;
class ScHeaderEditEngine;
class EditEngine;
class ScExtDocOptions;
class XclFontPropSetHelper;
class XclChPropSetHelper;
class XclTracer;
struct XclRootData {
  typedef std::shared_ptr< ScEditEngineDefaulter > ScEEDefaulterRef;
  typedef std::shared_ptr< ScHeaderEditEngine > ScHeaderEERef;
  typedef std::shared_ptr< EditEngine > EditEngineRef;
  typedef std::shared_ptr< XclFontPropSetHelper > XclFontPropSetHlpRef;
  typedef std::shared_ptr< XclChPropSetHelper > XclChPropSetHlpRef;
  typedef std::shared_ptr< ScExtDocOptions > ScExtDocOptRef;
  typedef std::shared_ptr< XclTracer > XclTracerRef;
  typedef std::shared_ptr< RootData > RootDataRef;
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
  LanguageType meSysLang;
  LanguageType meDocLang;
  LanguageType meUILang;
  short mnDefApiScript;
  ScAddress maScMaxPos;
  ScAddress maXclMaxPos;
  ScAddress maMaxPos;
  ScEEDefaulterRef mxEditEngine;
  ScHeaderEERef mxHFEditEngine;
  EditEngineRef mxDrawEditEng;
  XclFontPropSetHlpRef mxFontPropSetHlp;
  XclChPropSetHlpRef mxChPropSetHlp;
  ScExtDocOptRef mxExtDocOpt;
  XclTracerRef mxTracer;
  RootDataRef mxRD;
  virtual ~XclRootData();
};
class XclRoot {
public:
  virtual ~XclRoot();
  XclRootData &mrData;
};
class XclImpRoot : XclRoot {}; 
class XclImpColRowSettings : XclImpRoot {};
void lcl_ExportExcelBiff() {
XclRootData aExpData();
}
