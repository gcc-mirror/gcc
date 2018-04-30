// PR c++/31988
#include <new>

class C
{
public:
  void* operator new(std::size_t = 32) throw (std::bad_alloc); // { dg-error "first parameter" }
							       // { dg-error "dynamic exception specification" "" { target c++17 } .-1 }
							       // { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } .-2 }
  void* operator new[](std::size_t = 32) throw (std::bad_alloc); // { dg-error "first parameter" }
								 // { dg-error "dynamic exception specification" "" { target c++17 } .-1 }
								 // { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } .-2 }
  void* operator new(std::size_t = 32, const std::nothrow_t&) throw(); // { dg-error "first parameter" }
  void* operator new[](std::size_t = 32, const std::nothrow_t&) throw(); // { dg-error "first parameter" }
};

class D
{
public:
  void* operator new(std::size_t,
		     const std::nothrow_t& = std::nothrow_t()) throw();
  void* operator new[](std::size_t,
		       const std::nothrow_t& = std::nothrow_t()) throw();
};

class E
{
public:
  void* operator new(std::size_t = 0,
		     const std::nothrow_t& = std::nothrow_t()) throw(); // { dg-error "first parameter" }
  void* operator new[](std::size_t = 0,
		       const std::nothrow_t& = std::nothrow_t()) throw(); // { dg-error "first parameter" }
};
