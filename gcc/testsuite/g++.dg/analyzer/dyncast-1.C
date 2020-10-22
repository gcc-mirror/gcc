#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct base
{
  virtual ~base () {}
};
struct sub : public base
{
  int m_field;
};

int
test_1 (base *p)
{
  if (sub *q = dynamic_cast <sub*> (p))
    {
      __analyzer_dump_path (); // { dg-message "path" }
      return q->m_field;
    }
  return 0;
}
