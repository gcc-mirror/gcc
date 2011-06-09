#ifndef _OBJC_TEST_SUITE_NEXT_ENCODE_ASSIST_IMPL_H_
#define _OBJC_TEST_SUITE_NEXT_ENCODE_ASSIST_IMPL_H_

#ifdef __NEXT_RUNTIME__

/* Determine which API to use.  */
#include "next-abi.h"
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
#include <objc/runtime.h>
#else
#include <objc/objc-runtime.h>
#endif

/* ---- */

#undef  MAX
#undef  MIN
#undef  ROUND

#ifdef __cplusplus
#  define MAX(X, Y) ((X > Y) ? X : Y)
#  define MIN(X, Y) ((X < Y) ? X : Y)
#  define ROUND(V, A) (A * ((V + A - 1) / A))
#else
#  define MAX(X, Y)                    \
  ({ typeof (X) __x = (X), __y = (Y); \
     (__x > __y ? __x : __y); })
#  define MIN(X, Y)                    \
  ({ typeof (X) __x = (X), __y = (Y); \
     (__x < __y ? __x : __y); })
#  define ROUND(V, A) \
  ({ typeof (V) __v = (V); typeof (A) __a = (A); \
     __a * ((__v+__a - 1)/__a); })
#endif

#define BITS_PER_UNIT __CHAR_BIT__
typedef struct{ char a; } __small_struct;
#define STRUCTURE_SIZE_BOUNDARY (BITS_PER_UNIT * sizeof (__small_struct))

/*
  return the size of an object specified by type
*/

int
objc_sizeof_type (const char *type)
{
  /* Skip the variable name if any */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  switch (*type) {
  case _C_ID:
    return sizeof (id);
    break;

  case _C_CLASS:
    return sizeof (Class);
    break;

  case _C_SEL:
    return sizeof (SEL);
    break;

  case _C_CHR:
    return sizeof (char);
    break;

  case _C_UCHR:
    return sizeof (unsigned char);
    break;

  case _C_SHT:
    return sizeof (short);
    break;

  case _C_USHT:
    return sizeof (unsigned short);
    break;

  case _C_INT:
    return sizeof (int);
    break;

  case _C_UINT:
    return sizeof (unsigned int);
    break;

  case _C_LNG:
    return sizeof (long);
    break;

  case _C_ULNG:
    return sizeof (unsigned long);
    break;

  case _C_LNG_LNG:
    return sizeof (long long);
    break;

  case _C_ULNG_LNG:
    return sizeof (unsigned long long);
    break;

  case _C_FLT:
    return sizeof (float);
    break;

  case _C_DBL:
    return sizeof (double);
    break;

  case _C_PTR:
  case _C_ATOM:
  case _C_CHARPTR:
    return sizeof (char *);
    break;

  case _C_ARY_B:
    {
      int len = atoi (type + 1);
      while (isdigit ((unsigned char)*++type))
	;
      return len * objc_aligned_size (type);
    }
    break;

  case _C_BFLD:
    {
      /* The NeXT encoding of bitfields is _still_: b 'size' */
      int size = atoi (type + 1);
      /* Return an upper bound on byte size */
      return (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
    }

  case _C_STRUCT_B:
    {
      struct objc_struct_layout layout;
      unsigned int size;

      objc_layout_structure (type, &layout);
      while (objc_layout_structure_next_member (&layout))
        /* do nothing */ ;
      objc_layout_finish_structure (&layout, &size, NULL);

      return size;
    }

  case _C_UNION_B:
    {
      int max_size = 0;
      while (*type != _C_UNION_E && *type++ != '=')
	/* do nothing */;
      while (*type != _C_UNION_E)
	{
	  /* Skip the variable name if any */
	  if (*type == '"')
	    {
	      for (type++; *type++ != '"';)
		/* do nothing */;
	    }
	  max_size = MAX (max_size, objc_sizeof_type (type));
	  type = objc_skip_typespec (type);
	}
      return max_size;
    }
  }
  return 0; /* error */
}


/*
  Return the alignment of an object specified by type
*/

int
objc_alignof_type (const char *type)
{
  /* Skip the variable name if any */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }
  switch (*type) {
  case _C_ID:
    return __alignof__ (id);
    break;

  case _C_CLASS:
    return __alignof__ (Class);
    break;

  case _C_SEL:
    return __alignof__ (SEL);
    break;

  case _C_CHR:
    return __alignof__ (char);
    break;

  case _C_UCHR:
    return __alignof__ (unsigned char);
    break;

  case _C_SHT:
    return __alignof__ (short);
    break;

  case _C_USHT:
    return __alignof__ (unsigned short);
    break;

  case _C_INT:
  case _C_BFLD: /* This is for the NeXT only */
    return __alignof__ (int);
    break;

  case _C_UINT:
    return __alignof__ (unsigned int);
    break;

  case _C_LNG:
    return __alignof__ (long);
    break;

  case _C_ULNG:
    return __alignof__ (unsigned long);
    break;

  case _C_LNG_LNG:
    return __alignof__ (long long);
    break;

  case _C_ULNG_LNG:
    return __alignof__ (unsigned long long);
    break;

  case _C_FLT:
    return __alignof__ (float);
    break;

  case _C_DBL:
    return __alignof__ (double);
    break;

  case _C_PTR:
  case _C_ATOM:
  case _C_CHARPTR:
    return __alignof__ (char *);
    break;

  case _C_ARY_B:
    while (isdigit ((unsigned char)*++type))
      /* do nothing */;
    return objc_alignof_type (type);

  case _C_STRUCT_B:
    {
      struct objc_struct_layout layout;
      unsigned int align;

      objc_layout_structure (type, &layout);
      while (objc_layout_structure_next_member (&layout))
        /* do nothing */;
      objc_layout_finish_structure (&layout, NULL, &align);

      return align;
    }

  case _C_UNION_B:
    {
      int maxalign = 0;
      while (*type != _C_UNION_E && *type++ != '=')
	/* do nothing */;
      while (*type != _C_UNION_E)
	{
	  /* Skip the variable name if any */
	  if (*type == '"')
	    {
	      for (type++; *type++ != '"';)
		/* do nothing */;
	    }
	  maxalign = MAX (maxalign, objc_alignof_type (type));
	  type = objc_skip_typespec (type);
	}
      return maxalign;
    }
  }
  return 0; /* error */
}

/*
  The aligned size if the size rounded up to the nearest alignment.
*/

int
objc_aligned_size (const char *type)
{
  int size, align;

  /* Skip the variable name */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  size = objc_sizeof_type (type);
  align = objc_alignof_type (type);

  return ROUND (size, align);
}

/*
  The size rounded up to the nearest integral of the wordsize, taken
  to be the size of a void *.
*/

int
objc_promoted_size (const char *type)
{
  int size, wordsize;

  /* Skip the variable name */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  size = objc_sizeof_type (type);
  wordsize = sizeof (void *);

  return ROUND (size, wordsize);
}

/*
  Skip type qualifiers.  These may eventually precede typespecs
  occurring in method prototype encodings.
*/

const char *
objc_skip_type_qualifiers (const char *type)
{
  while (*type == _C_CONST
	 || *type == _C_IN
	 || *type == _C_INOUT
	 || *type == _C_OUT
	 || *type == _C_BYCOPY
         || *type == _C_BYREF
	 || *type == _C_ONEWAY
	 || *type == _C_GCINVISIBLE)
    {
      type += 1;
    }
  return type;
}

/*
  Skip one typespec element.  If the typespec is prepended by type
  qualifiers, these are skipped as well.
*/

const char *
objc_skip_typespec (const char *type)
{
  /* Skip the variable name if any */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  type = objc_skip_type_qualifiers (type);

  switch (*type) {

  case _C_ID:
    /* An id may be annotated by the actual type if it is known
       with the @"ClassName" syntax */

    if (*++type != '"')
      return type;
    else
      {
	while (*++type != '"')
	  /* do nothing */;
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
  case _C_LNG_LNG:
  case _C_ULNG_LNG:
  case _C_FLT:
  case _C_DBL:
  case _C_VOID:
  case _C_UNDEF:
    return ++type;
    break;

  case _C_ARY_B:
    /* skip digits, typespec and closing ']' */

    while (isdigit ((unsigned char)*++type))
      ;
    type = objc_skip_typespec (type);
    if (*type == _C_ARY_E)
      return ++type;
    else
      break; /* error */

  case _C_BFLD:
      /* The NeXT encoding for bitfields is _still_: b 'size' */
    while (isdigit ((unsigned char)*++type))
      ;	/* skip type and size */
    return type;

  case _C_STRUCT_B:
    /* skip name, and elements until closing '}'  */

    while (*type != _C_STRUCT_E && *type++ != '=')
      ;
    while (*type != _C_STRUCT_E)
      {
	type = objc_skip_typespec (type);
      }
    return ++type;

  case _C_UNION_B:
    /* skip name, and elements until closing ')'  */

    while (*type != _C_UNION_E && *type++ != '=')
      ;
    while (*type != _C_UNION_E)
      {
	type = objc_skip_typespec (type);
      }
    return ++type;

  case _C_PTR:
    /* Just skip the following typespec */

    return objc_skip_typespec (++type);
  }
  return 0; /* error */
}

/*
  Skip an offset as part of a method encoding.  This is prepended by a
  '+' if the argument is passed in registers.
*/
const char *
objc_skip_offset (const char *type)
{
  if (*type == '+')
    type++;
  while (isdigit ((unsigned char) *++type))
    ;
  return type;
}

/*
  Skip an argument specification of a method encoding.
*/
const char *
objc_skip_argspec (const char *type)
{
  type = objc_skip_typespec (type);
  type = objc_skip_offset (type);
  return type;
}

unsigned
objc_get_type_qualifiers (const char *type)
{
  unsigned res = 0;
  BOOL flag = YES;

  while (flag)
    switch (*type++)
      {
      case _C_CONST:	res |= _F_CONST; break;
      case _C_IN:	res |= _F_IN; break;
      case _C_INOUT:	res |= _F_INOUT; break;
      case _C_OUT:	res |= _F_OUT; break;
      case _C_BYCOPY:	res |= _F_BYCOPY; break;
      case _C_BYREF:  res |= _F_BYREF; break;
      case _C_ONEWAY:	res |= _F_ONEWAY; break;
      case _C_GCINVISIBLE: res |= _F_GCINVISIBLE; break;
      default: flag = NO;
    }

  return res;
}


/* The following three functions can be used to determine how a
   structure is laid out by the compiler. For example:

  struct objc_struct_layout layout;
  int i;

  objc_layout_structure (type, &layout);
  while (objc_layout_structure_next_member (&layout))
    {
      int position, align;
      const char *type;

      objc_layout_structure_get_info (&layout, &position, &align, &type);
      printf ("element %d has offset %d, alignment %d\n",
              i++, position, align);
    }

  These functions are used by objc_sizeof_type and objc_alignof_type
  functions to compute the size and alignment of structures. The
  previous method of computing the size and alignment of a structure
  was not working on some architectures, particulary on AIX, and in
  the presence of bitfields inside the structure. */
void
objc_layout_structure (const char *type,
                           struct objc_struct_layout *layout)
{
  const char *ntype;

  layout->original_type = ++type;

  /* Skip "<name>=" if any. Avoid embedded structures and unions. */
  ntype = type;
  while (*ntype != _C_STRUCT_E && *ntype != _C_STRUCT_B && *ntype != _C_UNION_B
         && *ntype++ != '=')
    /* do nothing */;

  /* If there's a "<name>=", ntype - 1 points to '='; skip the name */
  if (*(ntype - 1) == '=')
    type = ntype;

  layout->type = type;
  layout->prev_type = NULL;
  layout->record_size = 0;
  layout->record_align = MAX (BITS_PER_UNIT, STRUCTURE_SIZE_BOUNDARY);
}

BOOL
objc_layout_structure_next_member (struct objc_struct_layout *layout)
{
  register int desired_align = 0;

  /* The current type without the type qualifiers */
  const char *type;

  /* Add the size of the previous field to the size of the record.  */
  if (layout->prev_type)
    {
      type = objc_skip_type_qualifiers (layout->prev_type);

      if (*type != _C_BFLD)
        layout->record_size += objc_sizeof_type (type) * BITS_PER_UNIT;
      else
	layout->record_size += atoi (++type);
    }

  if (*layout->type == _C_STRUCT_E)
    return NO;

  /* Skip the variable name if any */
  if (*layout->type == '"')
    {
      for (layout->type++; *layout->type++ != '"';)
        /* do nothing */;
    }

  type = objc_skip_type_qualifiers (layout->type);

  desired_align = objc_alignof_type (type) * BITS_PER_UNIT;

  /* Record must have at least as much alignment as any field.
     Otherwise, the alignment of the field within the record
     is meaningless.  */
  layout->record_align = MAX (layout->record_align, desired_align);

  if (*type == _C_BFLD)
    {
      int bfld_size = atoi (++type);
      int int_align = __alignof__ (int) * BITS_PER_UNIT;
      /* If this bitfield would traverse a word alignment boundary, push it out 
	 to that boundary instead.  */
      if (layout->record_size % int_align
	  && (layout->record_size / int_align
	      < (layout->record_size + bfld_size - 1) / int_align))
	layout->record_size = ROUND (layout->record_size, int_align);
    }
  else if (layout->record_size % desired_align != 0)
    {
      /* We need to skip space before this field.
         Bump the cumulative size to multiple of field alignment.  */
      layout->record_size = ROUND (layout->record_size, desired_align);
    }

  /* Jump to the next field in record. */

  layout->prev_type = layout->type;
  layout->type = objc_skip_typespec (layout->type);      /* skip component */

  return YES;
}


void objc_layout_finish_structure (struct objc_struct_layout *layout,
                                   unsigned int *size,
                                   unsigned int *align)
{
  if (layout->type && *layout->type == _C_STRUCT_E)
    {
      /* Round the size up to be a multiple of the required alignment */
      layout->record_size = ROUND (layout->record_size, layout->record_align);
      layout->type = NULL;
    }
  if (size)
    *size = layout->record_size / BITS_PER_UNIT;
  if (align)
    *align = layout->record_align / BITS_PER_UNIT;
}


void objc_layout_structure_get_info (struct objc_struct_layout *layout,
                                     unsigned int *offset,
                                     unsigned int *align,
                                     const char **type)
{
  if (offset)
    *offset = layout->record_size / BITS_PER_UNIT;
  if (align)
    *align = layout->record_align / BITS_PER_UNIT;
  if (type)
    *type = layout->prev_type;
}

#endif /* __NEXT_RUNTIME__ */
#endif /* _OBJC_TEST_SUITE_NEXT_ENCODE_ASSIST_IMPL_H_ */
