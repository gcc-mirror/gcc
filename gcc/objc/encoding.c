/* Encoding of types for Objective C.
   Copyright (C) 1993, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "encoding.h"

#define MAX(X, Y)                    \
  ({ typeof(X) __x = (X), __y = (Y); \
     (__x > __y ? __x : __y); })

#define MIN(X, Y)                    \
  ({ typeof(X) __x = (X), __y = (Y); \
     (__x < __y ? __x : __y); })

#define ROUND(V, A) \
  ({ typeof(V) __v=(V); typeof(A) __a=(A); \
     __a*((__v+__a-1)/__a); })


static inline int
atoi (const char* str)
{
  int res = 0;
  
  while (isdigit (*str))
    res *= 10, res += (*str++ - '0');

  return res;
}

/*
  return the size of an object specified by type 
*/

int
objc_sizeof_type(const char* type)
{
  switch(*type) {
  case _C_ID:
    return sizeof(id);
    break;

  case _C_CLASS:
    return sizeof(Class);
    break;

  case _C_SEL:
    return sizeof(SEL);
    break;

  case _C_CHR:
    return sizeof(char);
    break;
    
  case _C_UCHR:
    return sizeof(unsigned char);
    break;

  case _C_SHT:
    return sizeof(short);
    break;

  case _C_USHT:
    return sizeof(unsigned short);
    break;

  case _C_INT:
    return sizeof(int);
    break;

  case _C_UINT:
    return sizeof(unsigned int);
    break;

  case _C_LNG:
    return sizeof(long);
    break;

  case _C_ULNG:
    return sizeof(unsigned long);
    break;

  case _C_FLT:
    return sizeof(float);
    break;

  case _C_DBL:
    return sizeof(double);
    break;

  case _C_VOID:
    return sizeof(void);
    break;
  case _C_PTR:
  case _C_ATOM:
  case _C_CHARPTR:
    return sizeof(char*);
    break;

  case _C_ARY_B:
    {
      int len = atoi(type+1);
      while (isdigit(*++type));
      return len*objc_aligned_size (type);
    }
    break; 

  case _C_STRUCT_B:
    {
      int acc_size = 0;
      int align;
      while (*type != _C_STRUCT_E && *type++ != '='); /* skip "<name>=" */
      while (*type != _C_STRUCT_E)
	{
	  align = objc_alignof_type (type);       /* padd to alignment */
	  acc_size = ROUND (acc_size, align);
	  acc_size += objc_sizeof_type (type);   /* add component size */
	  type = objc_skip_typespec (type);	         /* skip component */
	}
      return acc_size;
    }

  case _C_UNION_B:
    {
      int max_size = 0;
      while (*type != _C_UNION_E && *type++ != '=') /* do nothing */;
      while (*type != _C_UNION_E)
	{
	  max_size = MAX (max_size, objc_sizeof_type (type));
	  type = objc_skip_typespec (type);
	}
      return max_size;
    }
    
  default:
    {
      objc_error(nil, OBJC_ERR_BAD_TYPE, "unknown type %s\n", type);
      return 0;
    }
  }
}


/*
  Return the alignment of an object specified by type 
*/

int
objc_alignof_type(const char* type)
{
  switch(*type) {
  case _C_ID:
    return __alignof__(id);
    break;

  case _C_CLASS:
    return __alignof__(Class);
    break;
    
  case _C_SEL:
    return __alignof__(SEL);
    break;

  case _C_CHR:
    return __alignof__(char);
    break;
    
  case _C_UCHR:
    return __alignof__(unsigned char);
    break;

  case _C_SHT:
    return __alignof__(short);
    break;

  case _C_USHT:
    return __alignof__(unsigned short);
    break;

  case _C_INT:
    return __alignof__(int);
    break;

  case _C_UINT:
    return __alignof__(unsigned int);
    break;

  case _C_LNG:
    return __alignof__(long);
    break;

  case _C_ULNG:
    return __alignof__(unsigned long);
    break;

  case _C_FLT:
    return __alignof__(float);
    break;

  case _C_DBL:
    return __alignof__(double);
    break;

  case _C_PTR:
  case _C_ATOM:
  case _C_CHARPTR:
    return __alignof__(char*);
    break;

  case _C_ARY_B:
    while (isdigit(*++type)) /* do nothing */;
    return objc_alignof_type (type);
      
  case _C_STRUCT_B:
    {
      struct { int x; double y; } fooalign;
      while(*type != _C_STRUCT_E && *type++ != '=') /* do nothing */;
      if (*type != _C_STRUCT_E)
	return MAX (objc_alignof_type (type), __alignof__ (fooalign));
      else
	return __alignof__ (fooalign);
    }

  case _C_UNION_B:
    {
      int maxalign = 0;
      while (*type != _C_UNION_E && *type++ != '=') /* do nothing */;
      while (*type != _C_UNION_E)
	{
	  maxalign = MAX (maxalign, objc_alignof_type (type));
	  type = objc_skip_typespec (type);
	}
      return maxalign;
    }
    
  default:
    {
      objc_error(nil, OBJC_ERR_BAD_TYPE, "unknown type %s\n", type);
      return 0;
    }
  }
}

/*
  The aligned size if the size rounded up to the nearest alignment.
*/

int
objc_aligned_size (const char* type)
{
  int size = objc_sizeof_type (type);
  int align = objc_alignof_type (type);
  return ROUND (size, align);
}

/*
  The size rounded up to the nearest integral of the wordsize, taken
  to be the size of a void*.
*/

int 
objc_promoted_size (const char* type)
{
  int size = objc_sizeof_type (type);
  int wordsize = sizeof (void*);

  return ROUND (size, wordsize);
}

/*
  Skip type qualifiers.  These may eventually precede typespecs
  occurring in method prototype encodings.
*/

inline const char*
objc_skip_type_qualifiers (const char* type)
{
  while (*type == _C_CONST
	 || *type == _C_IN 
	 || *type == _C_INOUT
	 || *type == _C_OUT 
	 || *type == _C_BYCOPY
	 || *type == _C_ONEWAY)
    {
      type += 1;
    }
  return type;
}

  
/*
  Skip one typespec element.  If the typespec is prepended by type
  qualifiers, these are skipped as well.
*/

const char* 
objc_skip_typespec (const char* type)
{
  type = objc_skip_type_qualifiers (type);
  
  switch (*type) {

  case _C_ID:
    /* An id may be annotated by the actual type if it is known
       with the @"ClassName" syntax */

    if (*++type != '"')
      return type;
    else
      {
	while (*++type != '"') /* do nothing */;
	return type + 1;
      }

    /* The following are one character type codes */
  case _C_CLASS:
  case _C_SEL:
  case _C_CHR:
  case _C_UCHR:
  case _C_CHARPTR:
  case _C_ATOM:
  case _C_SHT:
  case _C_USHT:
  case _C_INT:
  case _C_UINT:
  case _C_LNG:
  case _C_ULNG:
  case _C_FLT:
  case _C_DBL:
  case _C_VOID:
  case _C_UNDEF:
    return ++type;
    break;

  case _C_ARY_B:
    /* skip digits, typespec and closing ']' */
    
    while(isdigit(*++type));
    type = objc_skip_typespec(type);
    if (*type == _C_ARY_E)
      return ++type;
    else
      {
	objc_error(nil, OBJC_ERR_BAD_TYPE, "bad array type %s\n", type);
	return 0;
      }

  case _C_STRUCT_B:
    /* skip name, and elements until closing '}'  */
    
    while (*type != _C_STRUCT_E && *type++ != '=');
    while (*type != _C_STRUCT_E) { type = objc_skip_typespec (type); }
    return ++type;

  case _C_UNION_B:
    /* skip name, and elements until closing ')'  */
    
    while (*type != _C_UNION_E && *type++ != '=');
    while (*type != _C_UNION_E) { type = objc_skip_typespec (type); }
    return ++type;

  case _C_PTR:
    /* Just skip the following typespec */
    
    return objc_skip_typespec (++type);
    
  default:
    {
      objc_error(nil, OBJC_ERR_BAD_TYPE, "unknown type %s\n", type);
      return 0;
    }
  }
}

/*
  Skip an offset as part of a method encoding.  This is prepended by a
  '+' if the argument is passed in registers.
*/
inline const char* 
objc_skip_offset (const char* type)
{
  if (*type == '+') type++;
  while(isdigit(*++type));
  return type;
}

/*
  Skip an argument specification of a method encoding.
*/
const char*
objc_skip_argspec (const char* type)
{
  type = objc_skip_typespec (type);
  type = objc_skip_offset (type);
  return type;
}

/*
  Return the number of arguments that the method MTH expects.
  Note that all methods need two implicit arguments `self' and
  `_cmd'. 
*/
int
method_get_number_of_arguments (struct objc_method* mth)
{
  int i = 0;
  const char* type = mth->method_types;
  while (*type)
    {
      type = objc_skip_argspec (type);
      i += 1;
    }
  return i - 1;
}

/*
  Return the size of the argument block needed on the stack to invoke
  the method MTH.  This may be zero, if all arguments are passed in
  registers.
*/

int
method_get_sizeof_arguments (struct objc_method* mth)
{
  const char* type = objc_skip_typespec (mth->method_types);
  return atoi (type);
}

/*
  Return a pointer to the next argument of ARGFRAME.  type points to
  the last argument.  Typical use of this look like:

  {
    char *datum, *type; 
    for (datum = method_get_first_argument (method, argframe, &type);
         datum; datum = method_get_next_argument (argframe, &type))
      {
        unsigned flags = objc_get_type_qualifiers (type);
        type = objc_skip_type_qualifiers (type);
	if (*type != _C_PTR)
          [portal encodeData: datum ofType: type];
	else
	  {
	    if ((flags & _F_IN) == _F_IN)
              [portal encodeData: *(char**)datum ofType: ++type];
	  }
      }
  }
*/  

char*
method_get_next_argument (arglist_t argframe,
			  const char **type)
{
  const char *t = objc_skip_argspec (*type);

  if (*t == '\0')
    return 0;

  *type = t;
  t = objc_skip_typespec (t);

  if (*t == '+')
    return argframe->arg_regs + atoi (++t);
  else
    return argframe->arg_ptr + atoi (t);
}

/*
  Return a pointer to the value of the first argument of the method 
  described in M with the given argumentframe ARGFRAME.  The type
  is returned in TYPE.  type must be passed to successive calls of 
  method_get_next_argument.
*/
char*
method_get_first_argument (struct objc_method* m,
			   arglist_t argframe, 
			   const char** type)
{
  *type = m->method_types;
  return method_get_next_argument (argframe, type);
}

/*
   Return a pointer to the ARGth argument of the method
   M from the frame ARGFRAME.  The type of the argument
   is returned in the value-result argument TYPE 
*/

char*
method_get_nth_argument (struct objc_method* m,
			 arglist_t argframe, int arg, 
			 const char **type)
{
  const char* t = objc_skip_argspec (m->method_types);

  if (arg > method_get_number_of_arguments (m))
    return 0;

  while (arg--)
    t = objc_skip_argspec (t);
  
  *type = t;
  t = objc_skip_typespec (t);

  if (*t == '+')
    return argframe->arg_regs + atoi (++t);
  else
    return argframe->arg_ptr + atoi (t);
}

unsigned
objc_get_type_qualifiers (const char* type)
{
  unsigned res = 0;
  BOOL flag = YES;

  while (flag)
    switch (*type++)
      {
      case _C_CONST:  res |= _F_CONST; break;
      case _C_IN:     res |= _F_IN; break;
      case _C_INOUT:  res |= _F_INOUT; break;
      case _C_OUT:    res |= _F_OUT; break;
      case _C_BYCOPY: res |= _F_BYCOPY; break;
      case _C_ONEWAY: res |= _F_ONEWAY; break;
      default: flag = NO;
    }

  return res;
}
