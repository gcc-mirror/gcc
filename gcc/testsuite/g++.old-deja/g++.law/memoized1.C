// Build don't link: 
// Special g++ Options: -fsave-memoized
// GROUPS passed memoized
class CArray
{
public:
  CArray();
};
class CBTree : public CArray
{
};
class CData
{
public:
  virtual               ~CData();
};
class CStr  : public CData
{
  inline int     Read();
  inline int     Write() const;
};
class CResource : private CBTree
{
  struct SResourceNode
  {
    CStr xKey;
  };
};
