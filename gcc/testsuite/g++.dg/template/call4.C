// PR c++/25364

struct OFX_PropertySuiteV1
{
  static int propGetDouble ();
};
template<int dimension,
	 class T,
	 int (*PROPGET)()
  >
struct OFX_AnimatedNumberParam
{
  virtual int paramSetValueAtTime()
  {
    return PROPGET();
  }
};
void  f()
{
  new OFX_AnimatedNumberParam<2,double,OFX_PropertySuiteV1::propGetDouble>();
}
