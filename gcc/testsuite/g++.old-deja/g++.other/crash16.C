// Build don't link:
// Special g++ Options: -O2
// Origin: scott snyder <snyder@fnal.gov>

// crash test - XFAIL *-*-*

void _S_initialize();

class locale
{
public:
  locale() throw()
  { _S_initialize (); }
};

void foo(const locale& __loc);

void print (const int&) 
{
  foo(locale());
}
