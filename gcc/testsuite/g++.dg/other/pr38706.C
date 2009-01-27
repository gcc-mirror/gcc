// PR target/38706
// { dg-do compile }
// { dg-options "-O2" }

class ios_base
{
public:
  virtual ~ios_base ();

};

class istrstream:virtual public ios_base
{
public:
  virtual ~istrstream ();

};

istrstream::~istrstream () {}
