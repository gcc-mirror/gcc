// Test if a local definition is in a linkonce/comdat section.
// { dg-do compile }
// { dg-final { scan-assembler "_ZTIP9CTemplateIhE\[: \t\n\]" } }
// { dg-final { scan-assembler-not "(.globl|.global)\[ 	\]+_ZTIP9CTemplateIhE" } }
// { dg-final { scan-assembler-not ".section\[^\n\r\]*_ZTIP9CTemplateIhE\[^\n\r\]*" } }


namespace std
{
  class type_info
  {
  protected:
    const char *__name;

  protected:
    explicit type_info(const char *__n): __name(__n) { }

  public:
    const char* name() const
    { return __name; }
  };
}

template<class TYPE>
class CTemplate
{
};

class CSecondModule {
public:
  CSecondModule();

private:
  const CTemplate<unsigned char> *m_variable; typedef CTemplate<unsigned char> m_variable_type;
};

CSecondModule::CSecondModule()
{
  typeid(const_cast<m_variable_type *>(m_variable)).name() != 0;
}
