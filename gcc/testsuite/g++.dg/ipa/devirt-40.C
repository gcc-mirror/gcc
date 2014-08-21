/* { dg-options "-O2 -fdump-tree-fre2-details"  } */
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
UnicodeString &
A::m_fn1 (UnicodeString &, int &p2, UErrorCode &) const
{
  UnicodeString a[2];
}

/* { dg-final { scan-tree-dump "converting indirect call to function virtual UnicodeString" "fre2"  } } */
/* { dg-final { cleanup-tree-dump "fre2" } } */
