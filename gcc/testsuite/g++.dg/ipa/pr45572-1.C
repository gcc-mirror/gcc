// { dg-do compile }
// { dg-options "-finline-small-functions -findirect-inlining -finline-functions -O" }

extern "C" {
typedef long unsigned int size_t;
typedef long int __ssize_t;
typedef struct _IO_FILE FILE;
typedef struct
{
} __mbstate_t;
extern __inline __attribute__ ((__gnu_inline__)) int
fgetc_unlocked (FILE *__fp)
{
}
extern __inline __attribute__ ((__gnu_inline__)) int
putc_unlocked (int __c, FILE *__stream)
{
}
extern __inline __attribute__ ((__gnu_inline__)) __ssize_t
getline (char **__lineptr, size_t *__n, FILE *__stream)
{
}
extern __inline __attribute__ ((__gnu_inline__)) int
ferror_unlocked (FILE *__stream) throw ()
{
}
}
typedef struct
{} __mpf_struct;
typedef __mpf_struct mpf_t[1];
typedef const __mpf_struct *mpf_srcptr;
typedef __mpf_struct *mpf_ptr;
extern "C" {
 void __gmpf_add (mpf_ptr, mpf_srcptr, mpf_srcptr);
}
class _knumber
{
 public:
  enum NumType {SpecialType, IntegerType, FractionType, FloatType};
  virtual NumType type(void) const = 0;
  virtual _knumber * add(_knumber const & arg2) const = 0;
  virtual operator long int(void) const = 0;
};
class _knumfloat : public _knumber
{
  _knumfloat(double num = 1.0)
  ;
  virtual NumType type(void) const ;
  virtual _knumber * add(_knumber const & arg2) const;
  virtual operator long int (void) const;
    mpf_t _mpf;
};
_knumber *_knumfloat::add(_knumber const & arg2) const
{
  if (arg2.type() == SpecialType)
    return arg2.add(*this);
{
    _knumfloat tmp_num(arg2);
    return tmp_num.add(*this);
  }
  _knumfloat * tmp_num = new _knumfloat();
  __gmpf_add(tmp_num->_mpf, _mpf,
   dynamic_cast<_knumfloat const &>(arg2)._mpf);
}
