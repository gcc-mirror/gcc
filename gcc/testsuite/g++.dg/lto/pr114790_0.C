// { dg-lto-do link }
// { dg-lto-options { { -w -flto -g -flto-partition=1to1 -O2 -shared -fPIC -fvisibility=hidden} } }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
struct APITracerContext {
  virtual ~APITracerContext() = default;
  virtual void releaseActivetracersList() = 0;
};
struct APITracerContextImp : APITracerContext {
  ~APITracerContextImp() override;
  void releaseActivetracersList() override;
};
struct APITracerContextImp globalAPITracerContextImp;
struct APITracerContextImp *pGlobalAPITracerContextImp = &globalAPITracerContextImp;
APITracerContextImp::~APITracerContextImp() {}

