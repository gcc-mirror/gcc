/* GNU Objective C Runtime archiving
   Copyright (C) 1993 Free Software Foundation, Inc.

Author: Kresten Krab Thorup

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

You should have received a copy of the GNU General Public License along with
   GNU CC; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

/*
** Note: This version assumes that int and longs are both 32bit.
*/

#ifndef __alpha__

#include "runtime.h"
#include "typedstream.h"

#define __objc_fatal(format, args...) \
 { fprintf(stderr, "archining: "); \
   fprintf(stderr, format, ## args); \
   fprintf(stderr, "\n"); abort(); }

/* Declare some functions... */

static int
objc_read_class (struct objc_typed_stream* stream, Class** class);

static int
objc_sizeof_type(const char* type);

static int
objc_write_use_common (struct objc_typed_stream* stream, unsigned int key);

static int
objc_write_register_common (struct objc_typed_stream* stream,
			    unsigned int key);

static int 
objc_write_class (struct objc_typed_stream* stream,
			 struct objc_class* class);

static const char*
__objc_skip_type (const char* type);

static void __objc_finish_write_root_object(struct objc_typed_stream*);
static void __objc_finish_read_root_object(struct objc_typed_stream*);

static __inline__ int
__objc_code_unsigned_char (unsigned char* buf, unsigned char val)
{
  if ((val&_B_VALUE) == val)
    {
      buf[0] = val|_B_SINT;
      return 1;
    }
  else
    {
      buf[0] = _B_NINT|0x01;
      buf[1] = val;
      return 2;
    }
}

int
objc_write_unsigned_char (struct objc_typed_stream* stream,
			  unsigned char value)
{
  unsigned char buf[sizeof (unsigned char)+1];
  int len = __objc_code_unsigned_char (buf, value);
  return (*stream->write)(stream->physical, buf, len);
}

static __inline__ int
__objc_code_char (unsigned char* buf, char val)
{
  if (val >= 0)
    return __objc_code_unsigned_char (buf, val);
  else
    {
      buf[0] = _B_NINT|_B_SIGN|0x01;
      buf[1] = -val;
      return 2;
    }
}

int
objc_write_char (struct objc_typed_stream* stream, char value)
{
  unsigned char buf[sizeof (char)+1];
  int len = __objc_code_char (buf, value);
  return (*stream->write)(stream->physical, buf, len);
}

static __inline__ int
__objc_code_unsigned_short (unsigned char* buf, unsigned short val)
{
  if (val <= 0xffU)
    return __objc_code_unsigned_char (buf, val);

  else 
    {
      buf[0] = _B_NINT|0x02;
      buf[1] = val/0x100;
      buf[2] = val%0x100;
      return 3;
    }
}

int
objc_write_unsigned_short (struct objc_typed_stream* stream, unsigned short value)
{
  unsigned char buf[sizeof (unsigned short)+1];
  int len = __objc_code_unsigned_short (buf, value);
  return (*stream->write)(stream->physical, buf, len);
}
      
static __inline__ int
__objc_code_short (unsigned char* buf, short val)
{
  if (val > 0)
    return __objc_code_unsigned_short (buf, val);

  if (val > -0x7f)		/* val > -128 */
    return __objc_code_char (buf, val);

  else 
    {
      int len = __objc_code_unsigned_short (buf, -val);
      buf[0] |= _B_SIGN;
      return len;
    }
}

int
objc_write_short (struct objc_typed_stream* stream, short value)
{
  unsigned char buf[sizeof (short)+1];
  int len = __objc_code_short (buf, value);
  return (*stream->write)(stream->physical, buf, len);
}
      

static __inline__ int
__objc_code_unsigned_int (unsigned char* buf, unsigned int val)
{
  if (val < 0x10000)
    return __objc_code_unsigned_short (buf, val%0x10000);

  else if (val < 0x1000000)
    {
      buf[0] = _B_NINT|3;
      buf[1] = val/0x10000;
      buf[2] = (val%0x10000)/0x100;
      buf[3] = val%0x100;
      return 4;
    }

  else 
    {
      buf[0] = _B_NINT|4;
      buf[1] = val/0x1000000;
      buf[2] = (val%0x1000000)/0x10000;
      buf[3] = (val%0x10000)/0x100;
      buf[4] = val%0x100;
      return 5;
    }
}

int
objc_write_unsigned_int (struct objc_typed_stream* stream, unsigned int value)
{
  unsigned char buf[sizeof(unsigned int)+1];
  int len = __objc_code_unsigned_int (buf, value);
  return (*stream->write)(stream->physical, buf, len);
}

static __inline__ int
__objc_code_int (unsigned char* buf, int val)
{
  if (val >= 0)
    return __objc_code_unsigned_int (buf, val);

  if (val > -0x7f)
    return __objc_code_char (buf, val);

  else 
    {
      int len = __objc_code_unsigned_int (buf, -val);
      buf[0] |= _B_SIGN;
      return len;
    }
}

int
objc_write_int (struct objc_typed_stream* stream, int value)
{
  unsigned char buf[sizeof(int)+1];
  int len = __objc_code_int (buf, value);
  return (*stream->write)(stream->physical, buf, len);
}

int
objc_write_string (struct objc_typed_stream* stream,
		   const unsigned char* string, unsigned int nbytes)
{
  unsigned char buf[sizeof(unsigned int)+1];
  int len = __objc_code_unsigned_int (buf, nbytes);
  
  if ((buf[0]&_B_CODE) == _B_SINT)
    buf[0] = (buf[0]&_B_VALUE)|_B_SSTR;

  else /* _B_NINT */
    buf[0] = (buf[0]&_B_VALUE)|_B_NSTR;

  if ((*stream->write)(stream->physical, buf, len) != 0)
    return (*stream->write)(stream->physical, string, nbytes);
  else
    return 0;
}

int
objc_write_string_atomic (struct objc_typed_stream* stream,
			  unsigned char* string, unsigned int nbytes)
{
  unsigned int key;
  if ((key = (unsigned int)hash_value_for_key (stream->stream_table, string)))
    return objc_write_use_common (stream, key);
  else
    {
      int length;
      hash_add (&stream->stream_table, (void*)key=(unsigned int)string, string);
      if ((length = objc_write_register_common (stream, key)))
	return objc_write_string (stream, string, nbytes);
      return length;
    }
}

static int
objc_write_register_common (struct objc_typed_stream* stream, unsigned int key)
{
  unsigned char buf[sizeof (unsigned int)+2];
  int len = __objc_code_unsigned_int (buf+1, key);
  if (len == 1)
    {
      buf[0] = _B_RCOMM|0x01;
      buf[1] &= _B_VALUE;
      return (*stream->write)(stream->physical, buf, len+1);
    }
  else
    {
      buf[1] = (buf[1]&_B_VALUE)|_B_RCOMM;
      return (*stream->write)(stream->physical, buf+1, len);
    }
}

static int
objc_write_use_common (struct objc_typed_stream* stream, unsigned int key)
{
  unsigned char buf[sizeof (unsigned int)+2];
  int len = __objc_code_unsigned_int (buf+1, key);
  if (len == 1)
    {
      buf[0] = _B_UCOMM|0x01;
      buf[1] &= _B_VALUE;
      return (*stream->write)(stream->physical, buf, 2);
    }
  else
    {
      buf[1] = (buf[1]&_B_VALUE)|_B_UCOMM;
      return (*stream->write)(stream->physical, buf+1, len);
    }
}

static __inline__ int
__objc_write_extension (struct objc_typed_stream* stream, unsigned char code)
{
  if (code <= _B_VALUE)
    {
      unsigned char buf = code|_B_EXT;
      return (*stream->write)(stream->physical, &buf, 1);
    }
  else 
    abort();
}

__inline__ int
__objc_write_object (struct objc_typed_stream* stream, id object)
{
  unsigned char buf = '\0';
  SEL write_sel = sel_get_uid ("write:");
  if (object)
    {
      __objc_write_extension (stream, _BX_OBJECT);
      objc_write_class (stream, object->class_pointer);
      (*objc_msg_lookup(object, write_sel))(object, write_sel, stream);
      return (*stream->write)(stream->physical, &buf, 1);
    }
  else
    return objc_write_use_common(stream, 0);
}

int 
objc_write_object_reference (struct objc_typed_stream* stream, id object)
{
  unsigned int key;
  if ((key = (unsigned int)hash_value_for_key (stream->object_table, object)))
    return objc_write_use_common (stream, key);

  __objc_write_extension (stream, _BX_OBJREF);
  return objc_write_unsigned_int (stream, (unsigned int)object);
}

int 
objc_write_root_object (struct objc_typed_stream* stream, id object)
{
  int len;
  if (stream->writing_root_p)
    __objc_fatal ("objc_write_root_object called recursively")
  else
    {
      stream->writing_root_p = 1;
      __objc_write_extension (stream, _BX_OBJROOT);
      if((len = objc_write_object (stream, object)))
	__objc_finish_write_root_object(stream);
      stream->writing_root_p = 0;
    }
  return len;
}

int 
objc_write_object (struct objc_typed_stream* stream, id object)
{
  unsigned int key;
  if ((key = (unsigned int)hash_value_for_key (stream->object_table, object)))
    return objc_write_use_common (stream, key);

  else if (object == nil)
    return objc_write_use_common(stream, 0);

  else
    {
      int length;
      hash_add (&stream->object_table, (void*)key=(unsigned int)object, object);
      if ((length = objc_write_register_common (stream, key)))
	return __objc_write_object (stream, object);
      return length;
    }
}

__inline__ int
__objc_write_class (struct objc_typed_stream* stream, struct objc_class* class)
{
  __objc_write_extension (stream, _BX_CLASS);
  objc_write_string_atomic(stream, (char*)class->name,
			   strlen((char*)class->name));
  return objc_write_unsigned_int (stream, CLS_GETNUMBER(class));
}


static int 
objc_write_class (struct objc_typed_stream* stream,
			 struct objc_class* class)
{
  unsigned int key;
  if ((key = (unsigned int)hash_value_for_key (stream->stream_table, class)))
    return objc_write_use_common (stream, key);
  else
    {
      int length;
      hash_add (&stream->stream_table, (void*)key=(unsigned int)class, class);
      if ((length = objc_write_register_common (stream, key)))
	return __objc_write_class (stream, class);
      return length;
    }
}


__inline__ int 
__objc_write_selector (struct objc_typed_stream* stream, SEL selector)
{
  const char* sel_name = sel_get_name (selector);
  __objc_write_extension (stream, _BX_SEL);
  return objc_write_string (stream, sel_name, strlen ((char*)sel_name));
}

int 
objc_write_selector (struct objc_typed_stream* stream, SEL selector)
{
  const char* sel_name = sel_get_name (selector);
  unsigned int key;
  if ((key = (unsigned int)hash_value_for_key (stream->stream_table, sel_name)))
    return objc_write_use_common (stream, key);
  else
    {
      int length;
      hash_add (&stream->stream_table, (void*)key=(unsigned int)sel_name, (char*)sel_name);
      if ((length = objc_write_register_common (stream, key)))
	return __objc_write_selector (stream, selector);
      return length;
    }
}



/*
** Read operations 
*/

__inline__ int
objc_read_char (struct objc_typed_stream* stream, char* val)
{
  unsigned char buf;
  int len;
  len = (*stream->read)(stream->physical, &buf, 1);
  if (len != 0)
    {
      if ((buf & _B_CODE) == _B_SINT)
	(*val) = (buf & _B_VALUE);

      else if ((buf & _B_NUMBER) == 1)
	{
	  len = (*stream->read)(stream->physical, val, 1);
	  if (buf&_B_SIGN)
	    (*val) = -1*(*val);
	}

      else
	__objc_fatal("expected 8bit signed int, got %dbit int",
		     (int)(buf&_B_NUMBER)*8);
    }
  return len;
}


__inline__ int
objc_read_unsigned_char (struct objc_typed_stream* stream, unsigned char* val)
{
  unsigned char buf;
  int len;
  if ((len = (*stream->read)(stream->physical, &buf, 1)))
    {
      if ((buf & _B_CODE) == _B_SINT)
	(*val) = (buf & _B_VALUE);

      else if ((buf & _B_NUMBER) == 1)
	len = (*stream->read)(stream->physical, val, 1);

      else
	__objc_fatal("expected 8bit unsigned int, got %dbit int",
		     (int)(buf&_B_NUMBER)*8);
    }
  return len;
}

__inline__ int
objc_read_short (struct objc_typed_stream* stream, short* value)
{
  unsigned char buf[sizeof(short)+1];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	{
	  int pos = 1;
	  int nbytes = buf[0] & _B_NUMBER;
	  if (nbytes > sizeof (short))
	    __objc_fatal("expected short, got bigger (%dbits)", nbytes*8);
	  len = (*stream->read)(stream->physical, buf+1, nbytes);
	  (*value) = 0;
	  while (pos <= nbytes)
	    (*value) = ((*value)*0x100) + buf[pos++];
	  if (buf[0] & _B_SIGN)
	    (*value) = -(*value);
	}
    }
  return len;
}

__inline__ int
objc_read_unsigned_short (struct objc_typed_stream* stream,
			  unsigned short* value)
{
  unsigned char buf[sizeof(unsigned short)+1];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	{
	  int pos = 1;
	  int nbytes = buf[0] & _B_NUMBER;
	  if (nbytes > sizeof (short))
	    __objc_fatal("expected short, got int or bigger");
	  len = (*stream->read)(stream->physical, buf+1, nbytes);
	  (*value) = 0;
	  while (pos <= nbytes)
	    (*value) = ((*value)*0x100) + buf[pos++];
	}
    }
  return len;
}


__inline__ int
objc_read_int (struct objc_typed_stream* stream, int* value)
{
  unsigned char buf[sizeof(int)+1];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	{
	  int pos = 1;
	  int nbytes = buf[0] & _B_NUMBER;
	  if (nbytes > sizeof (int))
	    __objc_fatal("expected int, got bigger");
	  len = (*stream->read)(stream->physical, buf+1, nbytes);
	  (*value) = 0;
	  while (pos <= nbytes)
	    (*value) = ((*value)*0x100) + buf[pos++];
	  if (buf[0] & _B_SIGN)
	    (*value) = -(*value);
	}
    }
  return len;
}

__inline__ int
__objc_read_nbyte_uint (struct objc_typed_stream* stream,
		       unsigned int nbytes, unsigned int* val)
{
  int len, pos = 0;
  unsigned char buf[sizeof(unsigned int)+1];

  if (nbytes > sizeof (int))
    __objc_fatal("expected int, got bigger");

  len = (*stream->read)(stream->physical, buf, nbytes);
  (*val) = 0;
  while (pos < nbytes)
    (*val) = ((*val)*0x100) + buf[pos++];
  return len;
}
  

__inline__ int
objc_read_unsigned_int (struct objc_typed_stream* stream,
			unsigned int* value)
{
  unsigned char buf[sizeof(unsigned int)+1];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	len = __objc_read_nbyte_uint (stream, (buf[0] & _B_VALUE), value);

    }
  return len;
}

__inline__ int
objc_read_string (struct objc_typed_stream* stream,
		  char** string)
{
  unsigned char buf[sizeof(unsigned int)+1];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      unsigned int key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register following */
	{
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read)(stream->physical, buf, 1);
	}

      switch (buf[0]&_B_CODE) {
      case _B_SSTR:
	{
	  int length = buf[0]&_B_VALUE;
	  (*string) = (char*)__objc_xmalloc(length+1);
	  if (key)
	    hash_add (&stream->stream_table, (void*)key, *string);
	  len = (*stream->read)(stream->physical, *string, length);
	  (*string)[length] = '\0';
	}
	break;

      case _B_UCOMM:
	{
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  (*string) = hash_value_for_key (stream->stream_table, (void*)key);
	}
	break;

      case _B_NSTR:
	{
	  unsigned int nbytes = buf[0]&_B_VALUE;
	  len = __objc_read_nbyte_uint(stream, nbytes, &nbytes);
	  if (len) {
	    (*string) = (char*)__objc_xmalloc(nbytes);
	    if (key)
	      hash_add (&stream->stream_table, (void*)key, *string);
	    len = (*stream->read)(stream->physical, *string, buf[0]&_B_VALUE);
	    (*string)[nbytes] = '\0';
	  }
	}
	break;
	
      default:
	__objc_fatal("expected string, got opcode %c\n", (buf[0]&_B_CODE));
      }
    }

  return len;
}


int
objc_read_object (struct objc_typed_stream* stream, id* object)
{
  unsigned char buf[sizeof (unsigned int)];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      SEL read_sel = sel_get_uid ("read:");
      unsigned int key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register common */
	{
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read)(stream->physical, buf, 1);
	}

      if (buf[0] == (_B_EXT | _BX_OBJECT))
	{
	  Class* class;

	  /* get class */
	  len = objc_read_class (stream, &class);

	  /* create instance */
	  (*object) = class_create_instance(class);

	  /* register? */
	  if (key)
	    hash_add (&stream->object_table, (void*)key, *object);

	  /* send -read: */
	  if (__objc_responds_to (*object, read_sel))
	    (*get_imp(class, read_sel))(*object, read_sel, stream);

	  /* check null-byte */
	  len = (*stream->read)(stream->physical, buf, 1);
	  if (buf[0] != '\0')
	    __objc_fatal("expected null-byte, got opcode %c", buf[0]);
	}

      else if ((buf[0]&_B_CODE) == _B_UCOMM)
	{
	  if (key)
	    __objc_fatal("cannot register use upcode...");
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  (*object) = hash_value_for_key (stream->object_table, (void*)key);
	}

      else if (buf[0] == (_B_EXT | _BX_OBJREF))	/* a forward reference */
	{
	  struct objc_list* other;
	  len = objc_read_unsigned_int (stream, &key);
	  other = (struct objc_list*)hash_value_for_key (stream->object_refs, (void*)key);
	  hash_add (&stream->object_refs, (void*)key, (void*)list_cons(object, other));
	}

      else if (buf[0] == (_B_EXT | _BX_OBJROOT)) /* a root object */
	{
	  if (key)
	    __objc_fatal("cannot register root object...");
	  len = objc_read_object (stream, object);
	  __objc_finish_read_root_object (stream);
	}

      else
	__objc_fatal("expected object, got opcode %c", buf[0]);
    }
  return len;
}

static int
objc_read_class (struct objc_typed_stream* stream, Class** class)
{
  unsigned char buf[sizeof (unsigned int)];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      unsigned int key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register following */
	{
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read)(stream->physical, buf, 1);
	}

      if (buf[0] == (_B_EXT | _BX_CLASS))
	{
	  char* class_name;
	  int version;

	  /* get class */
	  len = objc_read_string (stream, &class_name);
	  (*class) = objc_get_class(class_name);
	  free (class_name);

	  /* register */
	  if (key)
	    hash_add (&stream->stream_table, (void*)key, *class);

	  objc_read_unsigned_int(stream, &version);
	  hash_add (&stream->class_table, (*class)->name, (void*)version);
	}

      else if ((buf[0]&_B_CODE) == _B_UCOMM)
	{
	  if (key)
	    __objc_fatal("cannot register use upcode...");
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  (*class) = hash_value_for_key (stream->stream_table, (void*)key);
	  if (!*class)
	    __objc_fatal("cannot find class for key %x", key);
	}

      else
	__objc_fatal("expected class, got opcode %c", buf[0]);
    }
  return len;
}

int
objc_read_selector (struct objc_typed_stream* stream, SEL* selector)
{
  unsigned char buf[sizeof (unsigned int)];
  int len;
  if ((len = (*stream->read)(stream->physical, buf, 1)))
    {
      unsigned int key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register following */
	{
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read)(stream->physical, buf, 1);
	}

      if (buf[0] == (_B_EXT|_BX_SEL)) /* selector! */
	{
	  char* selector_name;

	  /* get selector */
	  len = objc_read_string (stream, &selector_name);
	  (*selector) = sel_get_uid(selector_name);
	  free (selector_name);

	  /* register */
	  if (key)
	    hash_add (&stream->stream_table, (void*)key, *selector);
	}

      else if ((buf[0]&_B_CODE) == _B_UCOMM)
	{
	  if (key)
	    __objc_fatal("cannot register use upcode...");
	  len = __objc_read_nbyte_uint(stream, (buf[0] & _B_VALUE), &key);
	  (*selector) = hash_value_for_key (stream->stream_table, (void*)key);
	}

      else
	__objc_fatal("expected selector, got opcode %c", buf[0]);
    }
  return len;
}

static int
objc_sizeof_type(const char* type)
{
  switch(*type) {
  case _C_ID: return sizeof(id);
    break;

  case _C_CLASS:
    return sizeof(Class*);
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
  case _C_LNG:
    return sizeof(int);
    break;

  case _C_UINT:
  case _C_ULNG:
    return sizeof(unsigned int);
    break;

  case _C_ATOM:
  case _C_CHARPTR:
    return sizeof(char*);
    break;

  default:
    fprintf(stderr, "objc_write_type: cannot parse typespec: %s\n", type);
    abort();
  }
}


static const char*
__objc_skip_type (const char* type)
{
  switch (*type) {
  case _C_ID:
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
    return ++type;
    break;

  case _C_ARY_B:
    while(isdigit(*++type));
    type = __objc_skip_type(type);
    if (*type == _C_ARY_E)
      return ++type;
    else
      __objc_fatal("cannot parse typespec: %s", type);
    break;

  default:
    fprintf(stderr, "objc_read_types: cannot parse typespec: %s\n", type);
    abort();
  }
}

/*
** USER LEVEL FUNCTIONS
*/

/*
** Write one object, encoded in TYPE and pointed to by DATA to the
** typed stream STREAM.  
*/

int
objc_write_type(TypedStream* stream, const char* type, const void* data)
{
  switch(*type) {
  case _C_ID:
    return objc_write_object (stream, *(id*)data);
    break;

  case _C_CLASS:
    return objc_write_class (stream, *(Class**)data);
    break;

  case _C_SEL:
    return objc_write_selector (stream, *(SEL*)data);
    break;

  case _C_CHR:
    return objc_write_char(stream, *(char*)data);
    break;
    
  case _C_UCHR:
    return objc_write_unsigned_char(stream, *(unsigned char*)data);
    break;

  case _C_SHT:
    return objc_write_short(stream, *(short*)data);
    break;

  case _C_USHT:
    return objc_write_unsigned_short(stream, *(unsigned short*)data);
    break;

  case _C_INT:
  case _C_LNG:
    return objc_write_int(stream, *(int*)data);
    break;

  case _C_UINT:
  case _C_ULNG:
    return objc_write_unsigned_int(stream, *(unsigned int*)data);
    break;

  case _C_CHARPTR:
    return objc_write_string (stream, (char*)data, strlen((char*)data));
    break;

  case _C_ATOM:
    return objc_write_string_atomic (stream, (char*)data, strlen((char*)data));
    break;

  case _C_ARY_B:
    {
      int len = atoi(type+1);
      while (isdigit(*++type));
      return objc_write_array (stream, type, len, data);
    }
    break; 

  default:
    fprintf(stderr, "objc_write_type: cannot parse typespec: %s\n", type);
    abort();
  }
}

/*
** Read one object, encoded in TYPE and pointed to by DATA to the
** typed stream STREAM.  DATA specifies the address of the types to
** read.  Expected type is checked against the type actually present
** on the stream. 
*/

int
objc_read_type(TypedStream* stream, const char* type, void* data)
{
  char c;
  switch(c = *type) {
  case _C_ID:
    return objc_read_object (stream, (id*)data);
    break;

  case _C_CLASS:
    return objc_read_class (stream, (Class**)data);
    break;

  case _C_SEL:
    return objc_read_selector (stream, (SEL*)data);
    break;

  case _C_CHR:
    return objc_read_char (stream, (char*)data);
    break;
    
  case _C_UCHR:
    return objc_read_unsigned_char (stream, (unsigned char*)data);
    break;

  case _C_SHT:
    return objc_read_short (stream, (short*)data);
    break;

  case _C_USHT:
    return objc_read_unsigned_short (stream, (unsigned short*)data);
    break;

  case _C_INT:
  case _C_LNG:
    return objc_read_int (stream, (int*)data);
    break;

  case _C_UINT:
  case _C_ULNG:
    return objc_read_unsigned_int (stream, (unsigned int*)data);
    break;

  case _C_CHARPTR:
  case _C_ATOM:
    return objc_read_string (stream, (char**)data);
    break;

  case _C_ARY_B:
    {
      int len = atoi(type+1);
      while (isdigit(*++type));
      return objc_read_array (stream, type, len, data);
    }
    break; 

  default:
    fprintf(stderr, "objc_write_type: cannot parse typespec: %s\n", type);
    abort();
  }
}

/*
** Write the object specified by the template TYPE to STREAM.  Last
** arguments specify addresses of values to be written.  It might 
** seem surprising to specify values by address, but this is extremely
** convenient for copy-paste with objc_read_types calls.  A more
** down-to-the-earth cause for this passing of addresses is that values
** of arbitrary size is not well supported in ANSI C for functions with
** variable number of arguments.
*/

int 
objc_write_types (TypedStream* stream, const char* type, ...)
{
  va_list args;
  const char *c;
  int res = 0;

  va_start(args, type);

  for (c = type; *c; c = __objc_skip_type (c))
    {
      switch(*c) {
      case _C_ID:
	res = objc_write_object (stream, *va_arg (args, id*));
	break;

      case _C_CLASS:
	res = objc_write_class (stream, *va_arg(args, Class**));
	break;

      case _C_SEL:
	res = objc_write_selector (stream, *va_arg(args, SEL*));
	break;
	
      case _C_CHR:
	res = objc_write_char (stream, *va_arg (args, char*));
	break;
	
      case _C_UCHR:
	res = objc_write_unsigned_char (stream,
					*va_arg (args, unsigned char*));
	break;
	
      case _C_SHT:
	res = objc_write_short (stream, *va_arg(args, short*));
	break;

      case _C_USHT:
	res = objc_write_unsigned_short (stream,
					 *va_arg(args, unsigned short*));
	break;

      case _C_INT:
      case _C_LNG:
	res = objc_write_int(stream, *va_arg(args, int*));
	break;
	
      case _C_UINT:
      case _C_ULNG:
	res = objc_write_unsigned_int(stream, *va_arg(args, unsigned int*));
	break;

      case _C_CHARPTR:
	{
	  char* str = va_arg(args, char*);
	  res = objc_write_string (stream, str, strlen(str));
	}
	break;

      case _C_ATOM:
	{
	  char* str = va_arg(args, char*);
	  res = objc_write_string_atomic (stream, str, strlen(str));
	}
	break;

      case _C_ARY_B:
	{
	  int len = atoi(c+1);
	  const char* t = c;
	  while (isdigit(*++t));
	  res = objc_write_array (stream, t, len, va_arg(args, void*));
	  t = __objc_skip_type (t);
	  if (*t != _C_ARY_E)
	    __objc_fatal("expected `]', got: %s", t);
	}
	break; 
	
      default:
	fprintf(stderr, "objc_write_type: cannot parse typespec: %s\n", type);
	abort();
      }
    }
  va_end(args);
  return res;
}


/* 
** Last arguments specify addresses of values to be read.  Expected
** type is checked against the type actually present on the stream. 
*/

int 
objc_read_types(TypedStream* stream, const char* type, ...)
{
  va_list args;
  const char *c;
  int res = 0;

  va_start(args, type);

  for (c = type; *c; c = __objc_skip_type(c))
    {
      switch(*c) {
      case _C_ID:
	res = objc_read_object(stream, va_arg(args, id*));
	break;

      case _C_CLASS:
	res = objc_read_class(stream, va_arg(args, Class**));
	break;

      case _C_SEL:
	res = objc_read_selector(stream, va_arg(args, SEL*));
	break;
	
      case _C_CHR:
	res = objc_read_char(stream, va_arg(args, char*));
	break;
	
      case _C_UCHR:
	res = objc_read_unsigned_char(stream, va_arg(args, unsigned char*));
	break;
	
      case _C_SHT:
	res = objc_read_short(stream, va_arg(args, short*));
	break;

      case _C_USHT:
	res = objc_read_unsigned_short(stream, va_arg(args, unsigned short*));
	break;

      case _C_INT:
      case _C_LNG:
	res = objc_read_int(stream, va_arg(args, int*));
	break;
	
      case _C_UINT:
      case _C_ULNG:
	res = objc_read_unsigned_int(stream, va_arg(args, unsigned int*));
	break;

      case _C_CHARPTR:
      case _C_ATOM:
	{
	  char** str = va_arg(args, char**);
	  res = objc_read_string (stream, str);
	}
	break;

      case _C_ARY_B:
	{
	  int len = atoi(c+1);
	  const char* t = c;
	  while (isdigit(*++t));
	  res = objc_read_array (stream, t, len, va_arg(args, void*));
	  t = __objc_skip_type (t);
	  if (*t != _C_ARY_E)
	    __objc_fatal("expected `]', got: %s", t);
	}
	break; 
	
      default:
	fprintf(stderr, "objc_read_type: cannot parse typespec: %s\n", type);
	abort();
      }
    }
  va_end(args);
  return res;
}

/*
** Write an array of COUNT elements of TYPE from the memory address DATA.
** This is equivalent of objc_write_type (stream, "[N<type>]", data)
*/

int
objc_write_array (TypedStream* stream, const char* type,
		  int count, const void* data)
{
  int off = objc_sizeof_type(type);
  const char* where = data;

  while (count-- > 0)
    {
      objc_write_type(stream, type, where);
      where += off;
    }
  return 1;
}

/*
** Read an array of COUNT elements of TYPE into the memory address
** DATA.  The memory pointed to by data is supposed to be allocated
** by the callee.  This is equivalent of 
**   objc_read_type (stream, "[N<type>]", data)
*/

int
objc_read_array (TypedStream* stream, const char* type,
		 int count, void* data)
{
  int off = objc_sizeof_type(type);
  char* where = (char*)data;

  while (count-- > 0)
    {
      objc_read_type(stream, type, where);
      where += off;
    }
  return 1;
}

static int 
__objc_fread(FILE* file, char* data, int len)
{
  return fread(data, len, 1, file);
}

static int 
__objc_fwrite(FILE* file, char* data, int len)
{
  return fwrite(data, len, 1, file);
}

static int
__objc_feof(FILE* file)
{
  return feof(file);
}

static int 
__objc_no_write(FILE* file, char* data, int len)
{
  __objc_fatal ("TypedStream not open for writing");
}

static int 
__objc_no_read(FILE* file, char* data, int len)
{
  __objc_fatal ("TypedStream not open for reading");
}

static int
__objc_read_typed_stream_signature (TypedStream* stream)
{
  char buffer[80];
  int pos = 0;
  do
    (*stream->read)(stream->physical, buffer+pos, 1);
  while (buffer[pos++] != '\0');
  sscanf (buffer, "GNU TypedStream %d", &stream->version);
  if (stream->version != OBJC_TYPED_STREAM_VERSION)
    __objc_fatal ("cannot handle TypedStream version %d", stream->version);
  return 1;
}

static int
__objc_write_typed_stream_signature (TypedStream* stream)
{
  char buffer[80];
  sprintf(buffer, "GNU TypedStream %d", OBJC_TYPED_STREAM_VERSION);
  stream->version = OBJC_TYPED_STREAM_VERSION;
  (*stream->write)(stream->physical, buffer, strlen(buffer)+1);
  return 1;
}

static void __objc_finish_write_root_object(struct objc_typed_stream* stream)
{
  hash_delete (stream->object_table);
  stream->object_table = hash_new(64,
				  (hash_func_type)hash_ptr,
				  (compare_func_type)compare_ptrs);
}

static void __objc_finish_read_root_object(struct objc_typed_stream* stream)
{
  node_ptr node;
  SEL awake_sel = sel_get_uid ("awake");

  /* resolve object forward references */
  for (node = hash_next (stream->object_refs, NULL); node;
       node = hash_next (stream->object_refs, node))
    {
      struct objc_list* reflist = node->value;
      const void* key = node->key;
      id object = hash_value_for_key (stream->object_table, key);
      while(reflist)
	{
	  *((id*)reflist->head) = object;
	  reflist = reflist->tail;
	}
      list_free (node->value);
    }

  /* empty object reference table */
  hash_delete (stream->object_refs);
  stream->object_refs = hash_new(8, (hash_func_type)hash_ptr,
				 (compare_func_type)compare_ptrs);
  
  /* call -awake for all objects read  */
  if (awake_sel)
    {
      for (node = hash_next (stream->object_table, NULL); node;
	   node = hash_next (stream->object_table, node))
	{
	  id object = node->value;
	  if (__objc_responds_to (object, awake_sel))
	    (*objc_msg_lookup(object, awake_sel))(object, awake_sel);
	}
    }

  /* empty object table */
  hash_delete (stream->object_table);
  stream->object_table = hash_new(64,
				  (hash_func_type)hash_ptr,
				  (compare_func_type)compare_ptrs);
}

/*
** Open the stream PHYSICAL in MODE
*/

TypedStream* 
objc_open_typed_stream (FILE* physical, int mode)
{
  int fflush(FILE*);

  TypedStream* s = (TypedStream*)__objc_xmalloc(sizeof(TypedStream));

  s->mode = mode;
  s->physical = physical;
  s->stream_table = hash_new(64,
			     (hash_func_type)hash_ptr,
			     (compare_func_type)compare_ptrs);
  s->object_table = hash_new(64,
			     (hash_func_type)hash_ptr,
			     (compare_func_type)compare_ptrs);
  s->eof = (objc_typed_eof_func)__objc_feof;
  s->flush = (objc_typed_flush_func)fflush;
  s->writing_root_p = 0;
  if (mode == OBJC_READONLY)
    {
      s->class_table = hash_new(8, (hash_func_type)hash_string,
				(compare_func_type)compare_strings);
      s->object_refs = hash_new(8, (hash_func_type)hash_ptr,
				(compare_func_type)compare_ptrs);
      s->read = (objc_typed_read_func)__objc_fread;
      s->write = (objc_typed_write_func)__objc_no_write;
      __objc_read_typed_stream_signature (s);
    }
  else if (mode == OBJC_WRITEONLY)
    {
      s->class_table = 0;
      s->object_refs = 0;
      s->read = (objc_typed_read_func)__objc_no_read;
      s->write = (objc_typed_write_func)__objc_fwrite;
      __objc_write_typed_stream_signature (s);
    }      
  else
    {
      objc_close_typed_stream (s);
      return NULL;
    }
  s->type = OBJC_FILE_STREAM;
  return s;
}

/*
** Open the file named by FILE_NAME in MODE
*/

TypedStream*
objc_open_typed_stream_for_file (const char* file_name, int mode)
{
  FILE* file = NULL;
  TypedStream* s;

  if (mode == OBJC_READONLY)
    file = fopen (file_name, "r");
  else
    file = fopen (file_name, "w");

  if (file)
    {
      s = objc_open_typed_stream (file, mode);
      if (s)
	s->type |= OBJC_MANAGED_STREAM;
      return s;
    }
  else
    return NULL;
}

/*
** Close STREAM freeing the structure it self.  If it was opened with 
** objc_open_typed_stream_for_file, the file will also be closed.
*/

void
objc_close_typed_stream (TypedStream* stream)
{
  if (stream->mode == OBJC_READONLY)
    {
      __objc_finish_read_root_object (stream); /* Just in case... */
      hash_delete (stream->class_table);
      hash_delete (stream->object_refs);
    }

  hash_delete (stream->stream_table);
  hash_delete (stream->object_table);

  if (stream->type == (OBJC_MANAGED_STREAM | OBJC_FILE_STREAM))
    fclose ((FILE*)stream->physical);

  free (stream);
}

BOOL
objc_end_of_typed_stream (TypedStream* stream)
{
  return (*stream->eof)(stream->physical);
}

void
objc_flush_typed_stream (TypedStream* stream)
{
  (*stream->flush)(stream->physical);
}

int 
objc_get_stream_class_version (TypedStream* stream, Class* class)
{
  if (stream->class_table)
    return (int) hash_value_for_key (stream->class_table, class->name);
  else
    return class_get_version (class);
}

#endif /* __alpha__ */
