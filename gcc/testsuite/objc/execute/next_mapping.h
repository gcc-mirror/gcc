/* This file "renames" various ObjC GNU runtime entry points
   (and fakes the existence of several others)
   if the NeXT runtime is being used.  */
/* Authors: Ziemowit Laski <zlaski@apple.com>  */
/*	    David Ayers <d.ayers@inode.at>  */

#ifdef __NEXT_RUNTIME__
#include <objc/objc-class.h>
#include <objc/Object.h>
#include <ctype.h>

#define objc_get_class(C)			objc_getClass(C)
#define objc_get_meta_class(C)			objc_getMetaClass(C)
#define class_get_class_method(C, S)		class_getClassMethod(C, S)
#define class_get_instance_method(C, S)		class_getInstanceMethod(C, S)
#define method_get_imp(M)			(((Method)M)->method_imp)
#define sel_get_name(S)				sel_getName(S)
#define class_create_instance(C)		class_createInstance(C, 0)
#define	class_get_class_name(C)			object_getClassName(C)
#define class_get_super_class(C)		(((struct objc_class *)C)->super_class)
#define object_get_super_class(O)		class_get_super_class(*(struct objc_class **)O)
#define objc_lookup_class(N)			objc_lookUpClass(N)
#define object_get_class(O)			(*(struct objc_class **)O)
#define class_is_class(C)			(CLS_GETINFO((struct objc_class *)C, CLS_CLASS)? YES: NO)
#define class_is_meta_class(C)			(CLS_GETINFO((struct objc_class *)C, CLS_META)? YES: NO)
#define object_is_class(O)			class_is_meta_class(*(struct objc_class **)O)
#define object_is_meta_class(O)			(class_is_meta_class(O) && class_is_meta_class(*(struct objc_class **)O))

/* You need either an empty +initialize method or an empty -forward:: method. 
   The NeXT runtime unconditionally sends +initialize to classes when they are 
   first used, and unconditionally tries to forward methods that the class 
   doesn't understand (including +initialize). If you have neither +initialize 
   nor -forward::, the runtime complains.  

   The simplest workaround is to add

      + initialize { return self; }

   to every root class @implementation.  */

#ifndef NULL
#define NULL 0
#endif

/* The following is necessary to "cover" the bf*.m test cases on NeXT.  */

#undef  MAX
#define MAX(X, Y)                    \
  ({ typeof (X) __x = (X), __y = (Y); \
     (__x > __y ? __x : __y); })

#undef  MIN
#define MIN(X, Y)                    \
  ({ typeof (X) __x = (X), __y = (Y); \
     (__x < __y ? __x : __y); })
  
#undef  ROUND
#define ROUND(V, A) \
  ({ typeof (V) __v = (V); typeof (A) __a = (A); \
     __a * ((__v+__a - 1)/__a); })

#define BITS_PER_UNIT __CHAR_BIT__
#define STRUCTURE_SIZE_BOUNDARY (BITS_PER_UNIT * sizeof (struct{char a;}))

/* Not sure why the following are missing from NeXT objc headers... */

#ifndef _C_LNG_LNG
#define _C_LNG_LNG  'q'
#endif
#ifndef _C_ULNG_LNG
#define _C_ULNG_LNG 'Q'
#endif
#ifndef _C_ATOM
#define _C_ATOM     '%'
#endif
#ifndef _C_BOOL
#define _C_BOOL     'B'
#endif

#define _C_CONST        'r'
#define _C_IN           'n'
#define _C_INOUT        'N'
#define _C_OUT          'o'
#define _C_BYCOPY       'O'
#define _C_BYREF        'R'
#define _C_ONEWAY       'V'
#define _C_GCINVISIBLE  '!'
   
#define _F_CONST        0x01
#define _F_IN           0x01
#define _F_OUT          0x02
#define _F_INOUT        0x03
#define _F_BYCOPY       0x04  
#define _F_BYREF        0x08  
#define _F_ONEWAY       0x10
#define _F_GCINVISIBLE  0x20

struct objc_struct_layout
{
  const char *original_type;
  const char *type;
  const char *prev_type;
  unsigned int record_size; 
  unsigned int record_align;
};

typedef union {
  char *arg_ptr;
  char arg_regs[sizeof (char*)];
} *arglist_t;                   /* argument frame */

const char *objc_skip_typespec (const char *type);
void objc_layout_structure_get_info (struct objc_struct_layout *layout,
    unsigned int *offset, unsigned int *align, const char **type);
void objc_layout_structure (const char *type,
    struct objc_struct_layout *layout);
BOOL objc_layout_structure_next_member (struct objc_struct_layout *layout);
void objc_layout_finish_structure (struct objc_struct_layout *layout,
    unsigned int *size, unsigned int *align);

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

  case _C_VOID:
    return sizeof (void);
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

inline const char *
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
inline const char *
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

/*
  Return the number of arguments that the method MTH expects.
  Note that all methods need two implicit arguments `self' and
  `_cmd'.
*/
int
method_get_number_of_arguments (struct objc_method *mth)
{
  int i = 0;
  const char *type = mth->method_types;
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
method_get_sizeof_arguments (struct objc_method *mth)
{
  const char *type = objc_skip_typespec (mth->method_types);
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
              [portal encodeData: *(char **) datum ofType: ++type];
	  }
      }
  }
*/

char *
method_get_next_argument (arglist_t argframe, const char **type)
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
char *
method_get_first_argument (struct objc_method *m,
			   arglist_t argframe,
			   const char **type)
{
  *type = m->method_types;
  return method_get_next_argument (argframe, type);
}

/*
   Return a pointer to the ARGth argument of the method
   M from the frame ARGFRAME.  The type of the argument
   is returned in the value-result argument TYPE
*/

char *
method_get_nth_argument (struct objc_method *m,
			 arglist_t argframe, int arg,
			 const char **type)
{
  const char *t = objc_skip_argspec (m->method_types);

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

  /* If there's a "<name>=", ntype - 1 points to '='; skip the the name */
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

/* A small, portable NSConstantString implementation for use with the NeXT
   runtime.
   
   On full-fledged Mac OS X systems, NSConstantString is provided
   as part of the Foundation framework.  However, on bare Darwin systems,
   Foundation is not included, and hence there is no NSConstantString 
   implementation to link against.

   This code is derived from the GNU runtime's NXConstantString implementation.
*/

struct objc_class _NSConstantStringClassReference;

@interface NSConstantString : Object
{
  char *c_string;
  unsigned int len;
}

-(const char *) cString;
-(unsigned int) length;

@end

@implementation NSConstantString

-(const char *) cString
{
  return (c_string);
}

-(unsigned int) length
{
  return (len);
}

@end

/* The NSConstantString metaclass will need to be initialized before we can
   send messages to strings.  */

void objc_constant_string_init (void) __attribute__((constructor));
void objc_constant_string_init (void) {
  memcpy (&_NSConstantStringClassReference,
	  objc_getClass ("NSConstantString"),
	  sizeof (_NSConstantStringClassReference));
}

#endif  /* #ifdef __NEXT_RUNTIME__ */
