/* { dg-options "-O2 -fdump-tree-fre3-details"  } */
typedef enum
{
} UErrorCode;
class UnicodeString
{
public:
  UnicodeString ();
  virtual ~UnicodeString ();
};
class A
{
  UnicodeString &m_fn1 (UnicodeString &, int &p2, UErrorCode &) const;
};
UnicodeString::UnicodeString () {}

UnicodeString g;

UnicodeString &
A::m_fn1 (UnicodeString &, int &p2, UErrorCode &) const
{
  UnicodeString a[2];
  return g;
}

/* { dg-final { scan-tree-dump-not "\\n  OBJ_TYPE_REF" "fre3"  } } */
