/* Verify that implicit and explicit calls to member operator new and delete
   are handled correctly.
   { dg-do compile }
   { dg-options "-Wmismatched-new-delete" } */

typedef __SIZE_TYPE__ size_t;

namespace std
{
#if __cplusplus >= 201703L
enum class align_val_t: size_t { };
#else
enum align_val_t { };
#endif

struct nothrow_t { };
const nothrow_t nothrow = { };
}

void sink (void*, ...);

struct POD
{
  void* operator new (size_t);
  void operator delete (void*);

  void* operator new[] (size_t);
  void operator delete[] (void*);
};

POD* nowarn_pod ()
{
  POD *p = new POD;
  delete p;
  return new POD;
}

void warn_pod_array_mismatch ()
{
  POD *p = new POD;
  delete[] p;                 // { dg-warning "'static void POD::operator delete \\\[]\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
  p = new POD[3];
  delete p;                   // { dg-warning "'static void POD::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
}


struct X1
{
  X1 ();

  void* operator new (size_t);
  void* operator new (size_t, std::align_val_t);
  void* operator new (size_t, std::nothrow_t) throw ();
  void* operator new (size_t, std::align_val_t, std::nothrow_t) throw ();

  void* operator new[] (size_t);
  void* operator new[] (size_t, std::align_val_t);
  void* operator new[] (size_t, std::nothrow_t) throw ();
  void* operator new[] (size_t, std::align_val_t, std::nothrow_t) throw ();

  void operator delete (void*);
  void operator delete (void*, size_t);
  void operator delete (void*, std::align_val_t);
  void operator delete (void*, size_t, std::align_val_t);
  void operator delete (void*, std::nothrow_t) throw ();
  void operator delete (void*, std::align_val_t, std::nothrow_t) throw ();

  void operator delete[] (void*);
  void operator delete[] (void*, size_t);
  void operator delete[] (void*, std::align_val_t);
  void operator delete[] (void*, size_t, std::align_val_t);
  void operator delete[] (void*, std::nothrow_t) throw ();
  void operator delete[] (void*, std::align_val_t, std::nothrow_t) throw ();
};

X1* nowarn_x1 ()
{
  return new X1;
}

X1* nowarn_x1_array ()
{
  return new X1[2];
}

X1* nowarn_align_val ()
{
  X1 *p = new (std::align_val_t (32)) X1;
  delete p;
  return new (std::align_val_t (64)) X1;
}

X1* nowarn_align_val_array ()
{
  X1 *p = new (std::align_val_t (32)) X1[2];
  delete[] p;
  return new (std::align_val_t (64)) X1[2];
}

X1* nowarn_x1_nothrow ()
{
  X1 *p = new (std::nothrow) X1;
  delete p;
  return new (std::nothrow) X1;
}

X1* nowarn_x1_nothrow_array ()
{
  X1 *p = new (std::nothrow) X1[3];
  delete[] p;
  return new (std::nothrow) X1[3];
}

X1* nowarn_align_val_nothrow ()
{
  X1 *p = new (std::align_val_t (32), std::nothrow) X1;
  delete p;
  return new (std::align_val_t (64), std::nothrow) X1;
}

X1* nowarn_align_val_nothrow_array ()
{
  X1 *p = new (std::align_val_t (32), std::nothrow) X1[4];
  delete[] p;
  return new (std::align_val_t (64), std::nothrow) X1[4];
}

void warn_x1_array_mismatch ()
{
  {
    X1 *p = new X1;
    delete[] p;               // { dg-warning "'static void X1::operator delete \\\[]\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
  }
  {
    X1 *p = new X1[2];
    delete p;                 // { dg-warning "'static void X1::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
  }
  {
    X1 *p = new (std::align_val_t (32)) X1[2];
    delete p;                 // { dg-warning "'static void X1::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
  }
  {
    // The following requires optimization (see warn_x1_array_mismatch()).
    X1 *p = new (std::nothrow) X1[3];
    delete p;                 // { dg-warning "'static void X1::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" "pr?????" { xfail *-*-* } }
  }
}

#pragma GCC push_options
#pragma GCC optimize "1"

void warn_x1_nothrow_array_mismatch ()
{
  X1 *p = new (std::nothrow) X1[3];
  delete p;                   // { dg-warning "'static void X1::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
}

#pragma GCC pop_options


struct X2: X1
{
  X2 ();

  void* operator new (size_t);
  void operator delete (void*);
};

X2* nowarn_x2 ()
{
  X2 *p = new X2;
  sink (p);
  return new X2;
}

void warn_x2 ()
{
  X1 *p = new X2;             // { dg-message "returned from 'static void\\* X2::operator new\\(size_t\\)'" "note" }
  sink (p);
  delete p;                   // { dg-warning "'static void X1::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
}

namespace N {
namespace NS {
namespace NmSpc {
namespace NameSpace {

namespace dl {   // same name as operator delete
namespace nw {   // and as operator new

struct X3: X2
{
  X3 ();

  void* operator new (size_t);
  void operator delete (void*);
};

X3* nowarn_x3 ()
{
  X3 *p = new X3;
  sink (p);
  return new X3;
}

void warn_x3 ()
{
  X1 *p = new X3;             // { dg-message "returned from 'static void\\* N::NS::NmSpc::NameSpace::dl::nw::X3::operator new\\(size_t\\)'" "note" }
  sink (p);
  delete p;                   // { dg-warning "'static void X1::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
}

template <int N>
struct X4: X2
{
  X4 ();

  void* operator new (size_t);
  void operator delete (void*);
};

void* nowarn_x4 ()
{
  X4<0> *p = new X4<0>;
  sink (p);
  return new X4<1>;
}

void warn_x4 ()
{
  X1 *p = new X4<1>;          // { dg-message "returned from 'static void\\* N::NS::NmSpc::NameSpace::dl::nw::X4<N>::operator new\\(size_t\\) \\\[with int N = 1]'" "note" }
  sink (p);
  delete p;                   // { dg-warning "'static void X1::operator delete\\(void\\*\\)' called on pointer returned from a mismatched allocation function" }
}

void warn_x4_inst_mismatch ()
{
  void *p = new X4<2>;        // { dg-message "returned from 'static void\\* N::NS::NmSpc::NameSpace::dl::nw::X4<N>::operator new\\(size_t\\) \\\[with int N = 2]'" "note" }
  sink (p);
  X4<3> *q = (X4<3>*)p;
  delete q;                   // { dg-warning "'static void N::NS::NmSpc::NameSpace::dl::nw::X4<N>::operator delete\\(void\\*\\) \\\[with int N = 3]' called on pointer returned from a mismatched allocation function" }
}

}   // nw
}   // dl
}   // NameSpace
}   // NmSpc
}   // NS
}   // N
