/* Utility macros to read Java(TM) .class files and byte codes.

   Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, February 1996. */

#ifndef JCF_H
#define JCF_H
#include "javaop.h"
#ifndef DEFUN
#if defined (__STDC__)
#define AND             ,
#define PTR             void *
#define DEFUN(name, arglist, args)      name(args)
#else
#define PTR             char *
#define AND             ;
#define DEFUN(name, arglist, args)      name arglist args;
#endif
#endif /* !DEFUN */

#ifndef PARAMS
#if defined (__STDC__)
#define PARAMS (paramlist)    paramlist
#else
#define PARAMS (paramlist)    ()
#endif
#endif

#ifndef JCF_u4
#define JCF_u4 unsigned long
#endif
#ifndef JCF_u2
#define JCF_u2 unsigned short
#endif

#define ALLOC xmalloc
#define REALLOC xrealloc
#ifndef FREE
#define FREE(PTR) free(PTR)
#endif

#ifdef JCF_word
#define JCF_word JCF_u4
#endif

#define JCF_ZIP    1
#define JCF_CLASS  2
#define JCF_SOURCE 3

struct JCF;
typedef int (*jcf_filbuf_t) PARAMS ((struct JCF*, int needed));

typedef struct CPool {
  /* Available number of elements in the constants array, before it
     must be re-allocated. */
  int capacity;

  /* The constant_pool_count. */
  int		count;

  uint8*	tags;

  jword*	data;
} CPool;

/* JCF encapsulates the state of reading a Java Class File. */

typedef struct JCF {
  unsigned char *buffer;
  unsigned char *buffer_end;
  unsigned char *read_ptr;
  unsigned char *read_end;
  int seen_in_zip;
  int java_source;
  long zip_offset;    
  jcf_filbuf_t filbuf;
  void *read_state;
  char *filename;
  char *classname;
  void *zipd;			/* Directory entry where it was found */
  JCF_u2 access_flags, this_class, super_class;
  CPool cpool;
} JCF;
/*typedef JCF*  JCF_FILE;*/

/* The CPOOL macros take a (pointer to a) CPool.
   The JPOOL macros take a (pointer to a) JCF.
   Some of the latter should perhaps be deprecated or removed. */

#define CPOOL_COUNT(CPOOL) ((CPOOL)->count)
#define JPOOL_SIZE(JCF) CPOOL_COUNT(&(JCF)->cpool)
#define JPOOL_TAG(JCF, INDEX) ((JCF)->cpool.tags[INDEX])
/* The INDEX'th constant pool entry as a JCF_u4. */
#define CPOOL_UINT(CPOOL, INDEX) ((CPOOL)->data[INDEX])
#define JPOOL_UINT(JCF, INDEX) CPOOL_UINT(&(JCF)->cpool, INDEX) /*deprecated*/
/* The first uint16 of the INDEX'th constant pool entry. */
#define CPOOL_USHORT1(CPOOL, INDEX) ((CPOOL)->data[INDEX] & 0xFFFF)
#define JPOOL_USHORT1(JCF, INDEX) CPOOL_USHORT1(&(JCF)->cpool, INDEX)
/* The second uint16 of the INDEX'th constant pool entry. */
#define CPOOL_USHORT2(CPOOL, INDEX) ((CPOOL)->data[INDEX] >> 16)
#define JPOOL_USHORT2(JCF, INDEX) CPOOL_USHORT2(&(JCF)->cpool, INDEX)
#define JPOOL_LONG(JCF, INDEX) \
  WORDS_TO_LONG (JPOOL_UINT(JCF, INDEX), JPOOL_UINT(JCF, (INDEX)+1))
#define JPOOL_DOUBLE(JCF, INDEX) \
  WORDS_TO_DOUBLE  (JPOOL_UINT(JCF, INDEX), JPOOL_UINT(JCF, (INDEX)+1))
#ifndef JPOOL_UTF_LENGTH
#define JPOOL_UTF_LENGTH(JCF, INDEX) \
  GET_u2 ((JCF)->buffer+JPOOL_UINT(JCF, INDEX))
#endif
#ifndef JPOOL_UTF_DATA
#define JPOOL_UTF_DATA(JCF, INDEX) \
  ((JCF)->buffer+JPOOL_UINT(JCF, INDEX)+2)
#endif
#define JPOOL_INT(JCF, INDEX) (WORD_TO_INT(JPOOL_UINT (JCF, INDEX)))
#define JPOOL_FLOAT(JCF, INDEX) WORD_TO_FLOAT (JPOOL_UINT (JCF, INDEX))

#define CPOOL_INDEX_IN_RANGE(CPOOL, INDEX) \
 ((INDEX) > 0 && (INDEX) < CPOOL_COUNT(CPOOL))

#define CPOOL_FINISH(CPOOL) { \
  if ((CPOOL)->tags) FREE ((CPOOL)->tags); \
  if ((CPOOL)->data) FREE ((CPOOL)->data); }

#define JCF_FINISH(JCF) { \
  CPOOL_FINISH(&(JCF)->cpool); \
  if ((JCF)->buffer) FREE ((JCF)->buffer); \
  if ((JCF)->filename) FREE ((JCF)->filename); \
  if ((JCF)->classname) FREE ((JCF)->classname); }
  
#define CPOOL_INIT(CPOOL) \
  ((CPOOL)->capacity = 0, (CPOOL)->count = 0, (CPOOL)->tags = 0, (CPOOL)->data = 0)

#define CPOOL_REINIT(CPOOL) ((CPOOL)->count = 0)

#define JCF_ZERO(JCF)  \
  ((JCF)->buffer = (JCF)->buffer_end = (JCF)->read_ptr = (JCF)->read_end = 0,\
   (JCF)->read_state = 0, (JCF)->filename = (JCF)->classname = 0, \
   CPOOL_INIT(&(JCF)->cpool), (JCF)->java_source = 0)

/* Given that PTR points to a 2-byte unsigned integer in network
   (big-endian) byte-order, return that integer. */
#define GET_u2(PTR) (((PTR)[0] << 8) | ((PTR)[1]))
/* Like GET_u2, but for little-endian format. */
#define GET_u2_le(PTR) (((PTR)[1] << 8) | ((PTR)[0]))

/* Given that PTR points to a 4-byte unsigned integer in network
   (big-endian) byte-order, return that integer. */
#define GET_u4(PTR) (((JCF_u4)(PTR)[0] << 24) | ((JCF_u4)(PTR)[1] << 16) \
  | ((JCF_u4)(PTR)[2] << 8) | ((JCF_u4)(PTR)[3]))
/* Like GET_u4, but for little-endian order. */
#define GET_u4_le(PTR) (((JCF_u4)(PTR)[3] << 24) | ((JCF_u4)(PTR)[2] << 16) \
  | ((JCF_u4)(PTR)[1] << 8) | ((JCF_u4)(PTR)[0]))

/* Make sure there are COUNT bytes readable. */
#define JCF_FILL(JCF, COUNT) \
  ((JCF)->read_end-(JCF)->read_ptr >= (COUNT) ? 0 : (*(JCF)->filbuf)(JCF, COUNT))
#define JCF_GETC(JCF) (JCF_FILL(JCF, 1) ? -1 : *(JCF)->read_ptr++)
#define JCF_READ(JCF, BUFFER, N) \
    (memcpy (BUFFER, (JCF)->read_ptr, N), (JCF)->read_ptr += (N))
#define JCF_SKIP(JCF,N) ((JCF)->read_ptr += (N))
#define JCF_readu(JCF) (*(JCF)->read_ptr++)

/* Reads an unsigned 2-byte integer in network (big-endian) byte-order
   from JCF.  Returns that integer.
   Does not check for EOF (make sure to call JCF_FILL before-hand). */
#define JCF_readu2(JCF) ((JCF)->read_ptr += 2, GET_u2 ((JCF)->read_ptr-2))
#define JCF_readu2_le(JCF) ((JCF)->read_ptr += 2, GET_u2_le((JCF)->read_ptr-2))

/* Like JCF_readu2, but read a 4-byte unsigned integer. */
#define JCF_readu4(JCF) ((JCF)->read_ptr += 4, GET_u4 ((JCF)->read_ptr-4))
#define JCF_readu4_le(JCF) ((JCF)->read_ptr += 4, GET_u4_le((JCF)->read_ptr-4))

#define JCF_TELL(JCF) ((JCF)->read_ptr - (JCF)->buffer)
#define JCF_SEEK(JCF, POS) ((JCF)->read_ptr = (JCF)->buffer + (POS))

#define ACC_PUBLIC 0x0001
#define ACC_PRIVATE 0x0002
#define ACC_PROTECTED 0x0004
#define ACC_STATIC 0x0008
#define ACC_FINAL 0x0010
#define ACC_SYNCHRONIZED 0x0020
#define ACC_SUPER 0x0020
#define ACC_VOLATILE 0x0040
#define ACC_TRANSIENT 0x0080
#define ACC_NATIVE 0x0100
#define ACC_INTERFACE 0x0200
#define ACC_ABSTRACT 0x0400

#define ACC_VISIBILITY (ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED)

#define CONSTANT_Class 7
#define CONSTANT_Fieldref 9
#define CONSTANT_Methodref 10
#define CONSTANT_InterfaceMethodref 11
#define CONSTANT_String 8
#define CONSTANT_Integer 3
#define CONSTANT_Float 4
#define CONSTANT_Long 5
#define CONSTANT_Double 6
#define CONSTANT_NameAndType 12
#define CONSTANT_Utf8 1
#define CONSTANT_Unicode 2

#define DEFAULT_CLASS_PATH "."

extern const char *find_class PARAMS ((const char *, int, JCF*, int));
extern const char *find_classfile PARAMS ((char *, JCF*, const char *));
extern int jcf_filbuf_from_stdio PARAMS ((JCF *jcf, int count));
extern int jcf_unexpected_eof PARAMS ((JCF*, int)) ATTRIBUTE_NORETURN;

/* Extract a character from a Java-style Utf8 string.
 * PTR points to the current character.
 * LIMIT points to the end of the Utf8 string.
 * PTR is incremented to point after the character thta gets returns.
 * On an error, -1 is returned. */
#define UTF8_GET(PTR, LIMIT) \
  ((PTR) >= (LIMIT) ? -1 \
   : *(PTR) < 128 ? *(PTR)++ \
   : (*(PTR)&0xE0) == 0xC0 && ((PTR)+=2)<=(LIMIT) && ((PTR)[-1]&0xC0) == 0x80 \
   ? (((PTR)[-2] & 0x1F) << 6) + ((PTR)[-1] & 0x3F) \
   : (*(PTR) & 0xF0) == 0xE0 && ((PTR) += 3) <= (LIMIT) \
   && ((PTR)[-2] & 0xC0) == 0x80 && ((PTR)[-1] & 0xC0) == 0x80 \
   ? (((PTR)[-3]&0x0F) << 12) + (((PTR)[-2]&0x3F) << 6) + ((PTR)[-1]&0x3F) \
   : ((PTR)++, -1))

extern char *jcf_write_base_directory;

/* Debug macros, for the front end */

extern int quiet_flag;
#ifdef VERBOSE_SKELETON
#undef SOURCE_FRONTEND_DEBUG
#define SOURCE_FRONTEND_DEBUG(X)				\
  {if (!quiet_flag) {printf ("* "); printf X; putchar ('\n');} }
#else
#define SOURCE_FRONTEND_DEBUG(X)
#endif

/* Declarations for dependency code.  */
extern void jcf_dependency_reset PARAMS ((void));
extern void jcf_dependency_set_target PARAMS ((const char *));
extern void jcf_dependency_add_target PARAMS ((const char *));
extern void jcf_dependency_set_dep_file PARAMS ((const char *));
extern void jcf_dependency_add_file PARAMS ((const char *, int));
extern void jcf_dependency_write PARAMS ((void));
extern void jcf_dependency_init PARAMS ((int));

/* Declarations for path handling code.  */
extern void jcf_path_init PARAMS ((void));
extern void jcf_path_classpath_arg PARAMS ((const char *));
extern void jcf_path_CLASSPATH_arg PARAMS ((const char *));
extern void jcf_path_include_arg PARAMS ((const char *));
extern void jcf_path_seal PARAMS ((void));
extern void *jcf_path_start PARAMS ((void));
extern void *jcf_path_next PARAMS ((void *));
extern char *jcf_path_name PARAMS ((void *));
extern int jcf_path_is_zipfile PARAMS ((void *));
extern int jcf_path_is_system PARAMS ((void *));
extern int jcf_path_max_len PARAMS ((void));

#endif
