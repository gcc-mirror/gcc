/* PR middle-end/101791 - missing warning on a mismatch between scalar
   and array forms of new and delete
   { dg-do compile }
   { dg-options "-Wall" } */

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


void sink (void*, ...);


void nowarn_scalar_scalar ()
{
  {
    int *p = new int;
    sink (p);
    delete p;
  }

  {
    int *p = new (std::align_val_t (8)) int;
    sink (p);
    delete p;
  }

  {
    int *p = new (std::nothrow) int;
    sink (p);
    delete p;
  }

  {
    int *p = new (std::align_val_t (8), std::nothrow) int;
    sink (p);
    delete p;
  }
}

void nowarn_array_array ()
{
  {
    int *p = new int[__LINE__];
    sink (p);
    delete[] p;
  }

  {
    int *p = new (std::align_val_t (8)) int[__LINE__];
    sink (p);
    delete[] p;
  }

  {
    int *p = new (std::nothrow) int[__LINE__];
    sink (p);
    delete[] p;
  }

  {
    int *p = new (std::align_val_t (8), std::nothrow) int[__LINE__];
    sink (p);
    delete[] p;
  }
}



void nowarn_scalar_array ()
{
  {
    int *p = new int;   // { dg-message "returned from" }
    sink (p);
    delete[] p;         // { dg-warning "\\\[-Wmismatched-new-delete" }
  }

  {
    int *p = new (std::align_val_t (8)) int;
    sink (p);
    delete[] p;         // { dg-warning "\\\[-Wmismatched-new-delete" }
  }

  {
    int *p = new (std::nothrow) int;
    sink (p);
    delete[] p;         // { dg-warning "\\\[-Wmismatched-new-delete" }
  }

  {
    int *p = new (std::align_val_t (8), std::nothrow) int;
    sink (p);
    delete[] p;         // { dg-warning "\\\[-Wmismatched-new-delete" }
  }
}


void nowarn_array_scalar ()
{
  {
    int *p = new int[__LINE__];
    sink (p);
    delete p;           // { dg-warning "\\\[-Wmismatched-new-delete" }
  }

  {
    int *p = new (std::align_val_t (8)) int[__LINE__];
    sink (p);
    delete p;           // { dg-warning "\\\[-Wmismatched-new-delete" }
  }

  {
    int *p = new (std::nothrow) int[__LINE__];
    sink (p);
    delete p;           // { dg-warning "\\\[-Wmismatched-new-delete" }
  }

  {
    int *p = new (std::align_val_t (8), std::nothrow) int[__LINE__];
    sink (p);
    delete p;           // { dg-warning "\\\[-Wmismatched-new-delete" }
  }
}
