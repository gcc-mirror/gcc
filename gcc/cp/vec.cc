// new abi support -*- C++ -*-
// Copyright (C) 2000
// Free Software Foundation, Inc.
// Written by Nathan Sidwell, Codesourcery LLC, <nathan@codesourcery.com>
// 
// GNU CC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// GNU CC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU CC; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA. 

// As a special exception, if you link this library with other files,
// some of which are compiled with GCC, to produce an executable,
// this library does not by itself cause the resulting executable
// to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

#if defined(__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
#include <cxxabi.h>

namespace __cxxabiv1
{

/* allocate and construct array */
void *
__cxa_vec_new (size_t element_count,
               size_t element_size,
               size_t padding_size,
               void (*constructor) (void *),
               void (*destructor) (void *))
{
  size_t size = element_count * element_size + padding_size;
  char *base = static_cast <char *> (operator new[] (size));
  
  if (padding_size)
    {
      base += padding_size;
      reinterpret_cast <size_t *> (base)[-1] = element_count;
    }
  try
    {
      __cxa_vec_ctor (base, element_count, element_size,
                      constructor, destructor);
    }
  catch (...)
    {
      operator delete[] (base - padding_size);
      throw;
    }
  return base;
}

/* construct array */
void
__cxa_vec_ctor (void *array_address,
                size_t element_count,
                size_t element_size,
                void (*constructor) (void *),
                void (*destructor) (void *))
{
  size_t ix = 0;
  char *ptr = static_cast <char *> (array_address);
  
  try
    {
      if (constructor)
        for (; ix != element_count; ix++, ptr += element_size)
          constructor (ptr);
    }
  catch (...)
    {
      try
        {
          if (destructor)
            for (; ix--; ptr -= element_size)
              destructor (ptr);
        }
      catch (...)
        {
          // [except.ctor]/3 If a destructor called during stack unwinding
          // exists with an exception, terminate is called.
          std::terminate ();
        }
      throw;
    }
}

/* destruct array */
void
__cxa_vec_dtor (void *array_address,
                size_t element_count,
                size_t element_size,
                void (*destructor) (void *))
{
  if (destructor)
    {
      char *ptr = static_cast <char *> (array_address);
      
      ptr += element_count * element_size;
      
      for (size_t ix = element_count; ix--;)
        {
          ptr -= element_size;
          destructor (ptr);
        }
    }
}

/* destruct and release array */
void
__cxa_vec_delete (void *array_address,
                  size_t element_size,
                  size_t padding_size,
                  void (*destructor) (void *))
{
  char *base = static_cast <char *> (array_address);
  
  if (padding_size)
    {
      size_t element_count = reinterpret_cast <size_t *> (base)[-1];
      
      __cxa_vec_dtor (base, element_count, element_size, destructor);
      base -= padding_size;
    }
  operator delete[] (base);
}

} // namespace __cxxabiv1

#endif // defined(__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
