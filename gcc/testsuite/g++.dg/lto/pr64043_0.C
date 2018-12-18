// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -flto -std=c++11 } } }
// { dg-extra-ld-options "-r -nostdlib -O2" }
class Validator
{
public:
  virtual ~Validator ();
};
class FooWriter
{
  Validator *validator;
  ~FooWriter ();
};
FooWriter::~FooWriter () { delete validator; }
