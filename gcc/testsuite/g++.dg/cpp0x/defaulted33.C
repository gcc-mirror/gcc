// PR c++/50973
// { dg-do compile { target c++11 } }

class HD
{
  public:
  virtual ~HD() {};
};
class InputHD : public virtual HD
{
};
class OutputHD : public virtual HD
{
};
class IOHD : public InputHD, public OutputHD
{
};
template <typename T, unsigned int N>
class ArrayNHD : public IOHD
{
  public:
  ~ArrayNHD() = default;
};
class TLText
{
  ~TLText();
  ArrayNHD<int, 1>* m_argsHD;
};
TLText::~TLText()
{
  delete m_argsHD;
}
