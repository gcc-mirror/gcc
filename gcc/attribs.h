/* Declarations and definitions dealing with attribute handling.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_ATTRIBS_H
#define GCC_ATTRIBS_H

extern const struct attribute_spec *lookup_attribute_spec (const_tree);
extern void init_attributes (void);

/* Process the attributes listed in ATTRIBUTES and install them in *NODE,
   which is either a DECL (including a TYPE_DECL) or a TYPE.  If a DECL,
   it should be modified in place; if a TYPE, a copy should be created
   unless ATTR_FLAG_TYPE_IN_PLACE is set in FLAGS.  FLAGS gives further
   information, in the form of a bitwise OR of flags in enum attribute_flags
   from tree.h.  Depending on these flags, some attributes may be
   returned to be applied at a later stage (for example, to apply
   a decl attribute to the declaration rather than to its type).  */
extern tree decl_attributes (tree *, tree, int);

extern bool cxx11_attribute_p (const_tree);
extern tree get_attribute_name (const_tree);
extern void apply_tm_attr (tree, tree);
extern tree make_attribute (const char *, const char *, tree);

extern struct scoped_attributes* register_scoped_attributes (const struct attribute_spec *,
							     const char *);

extern char *sorted_attr_string (tree);
extern bool common_function_versions (tree, tree);
extern char *make_unique_name (tree, const char *, bool);
extern tree make_dispatcher_decl (const tree);
extern bool is_function_default_version (const tree);

/* For a given IDENTIFIER_NODE, strip leading and trailing '_' characters
   so that we have a canonical form of attribute names.  */

static inline tree
canonicalize_attr_name (tree attr_name)
{
  const size_t l = IDENTIFIER_LENGTH (attr_name);
  const char *s = IDENTIFIER_POINTER (attr_name);

  if (l > 4 && s[0] == '_' && s[1] == '_' && s[l - 1] == '_' && s[l - 2] == '_')
    return get_identifier_with_length (s + 2, l - 4);

  return attr_name;
}

/* Compare attribute identifiers ATTR1 and ATTR2 with length ATTR1_LEN and
   ATTR2_LEN.  */

static inline bool
cmp_attribs (const char *attr1, size_t attr1_len,
	     const char *attr2, size_t attr2_len)
{
  return attr1_len == attr2_len && strncmp (attr1, attr2, attr1_len) == 0;
}

/* Compare attribute identifiers ATTR1 and ATTR2.  */

static inline bool
cmp_attribs (const char *attr1, const char *attr2)
{
  return cmp_attribs (attr1, strlen (attr1), attr2, strlen (attr2));
}

/* Given an identifier node IDENT and a string ATTR_NAME, return true
   if the identifier node is a valid attribute name for the string.  */

static inline bool
is_attribute_p (const char *attr_name, const_tree ident)
{
  return cmp_attribs (attr_name, strlen (attr_name),
		      IDENTIFIER_POINTER (ident), IDENTIFIER_LENGTH (ident));
}

#endif // GCC_ATTRIBS_H
