/* Routines dealing with ObjC encoding of types
   Copyright (C) 1992-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_OBJC_ENCODING_H
#define GCC_OBJC_ENCODING_H

/* This is used to initialize the obstacks used by encoding.  It
   should be called before any encoding function is used.  It is
   usually done in objc_init().  */
extern void objc_encoding_init (void);

/* Encode a method prototype.  The format is described in
   gcc/doc/objc.texi, section 'Method signatures'.  */
extern tree encode_method_prototype (tree method_decl);

/* This is used to implement @encode().  See gcc/doc/objc.texi,
   section '@encode'.  */
extern tree objc_build_encode_expr (tree type);

/* (Decide if these can ever be validly changed.)  */
#define OBJC_ENCODE_INLINE_DEFS		0
#define OBJC_ENCODE_DONT_INLINE_DEFS	1

/* Encode the attributes of a property.  */
extern tree objc_v2_encode_prop_attr (tree property);

/* Encode the type of a field.  Return an identifier with the type
   encoding for the field.  The type encoding is a null-terminated
   string.  */
extern tree encode_field_decl (tree field_decl);

/* Tells "encode_pointer/encode_aggregate" whether we are generating
   type descriptors for instance variables (as opposed to methods).
   Type descriptors for instance variables contain more information
   than methods (for static typing and embedded structures).

   TODO: Replace this global variable with an argument that is passed
   to the various encode() functions.

   TODO: Change it to a 'bool'.  */
extern int generating_instance_variables;

#endif /* GCC_OBJC_ENCODING_H */
