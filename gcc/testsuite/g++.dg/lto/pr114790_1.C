struct APITracerContext {
  virtual void releaseActivetracersList() = 0;
};
extern struct APITracerContextImp *pGlobalAPITracerContextImp;
struct APITracerContextImp : APITracerContext { void releaseActivetracersList();};
int g();
inline int
apiTracerWrapperImp(  ) {
  for (int i = 0; i < g(); i++) 
  pGlobalAPITracerContextImp->releaseActivetracersList();
}
__attribute__((visibility("default"))) int
zeCommandListAppendMemoryCopyTracing() {
  return apiTracerWrapperImp(      );
}
