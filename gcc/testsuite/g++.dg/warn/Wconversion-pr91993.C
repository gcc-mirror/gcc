// PR c++/91993
// { dg-do compile }
// { dg-options "-Wconversion" }

typedef unsigned char uc;

int
foo (const uc &a, const uc &b, const uc &c)
{
  return static_cast<uc>(static_cast<uc>(a << 1U) | b) | c;		// { dg-bogus "conversion from 'int' to 'unsigned char' may change value" }
}

int
bar (const uc &a, const uc &b, const uc &c, int &d)
{
  return static_cast<uc>(static_cast<uc>((d++, a) << 1U) | b) | c;	// { dg-bogus "conversion from 'int' to 'unsigned char' may change value" }
}
