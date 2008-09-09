/* { dg-do compile } */

class GenericClass;
struct AlsaDriver
{
  virtual int _read (unsigned nframes);
};
typedef void (GenericClass::*GenericMemFuncType) ();
GenericMemFuncType m_pFunction;
void AlsaDriver1 ()
{
  m_pFunction = reinterpret_cast < GenericMemFuncType > (&AlsaDriver::_read);
}

