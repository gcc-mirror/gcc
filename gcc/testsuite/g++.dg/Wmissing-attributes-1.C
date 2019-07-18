// { dg-do compile }
// { dg-options "-Wmissing-attributes" }

#define ATTR(list)   __attribute__ (list)

/* Type attributes are normally absent in template functions, and the
   mere presence of any such attribute used to cause the
   -Wmissing-attributes checks, that checked for attributes typically
   associated with functions rather than types, to report any missing
   attributes twice: once for the specialization attribute list, once
   for its type attribute list.

   This test uses both decl and type attributes to exercise the code
   that avoids reporting duplicates, in ways that failed in the past
   but that were not covered in other tests.  */
typedef void* ATTR ((alloc_size (1))) f_type (int);

template <class T>
f_type
ATTR ((malloc))
missing_malloc;            // { dg-message "missing primary template attribute .malloc." }

template <>
f_type
missing_malloc<char>;      // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }


/* Check that even an attribute that appears in both lists (decl and
   type) in a template declaration is reported as missing only
   once.  */

template <class T>
f_type
ATTR ((alloc_size (1))) // In both attr lists, decl's and type's.
missing_alloc_size;            // { dg-message "missing primary template attribute .alloc_size." }

template <>
void *
missing_alloc_size<char>(int); // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }


/* Check that even an attribute that appears in both lists (decl and
   type) is not reported as missing if it's present only in the type
   list.  */

template <class T>
f_type
ATTR ((alloc_size (1))) // In both attr lists, decl's and type's.
missing_nothing;

template <>
f_type
missing_nothing<char>;


/* For completeness, check that a type attribute is matched by a decl
   attribute in the specialization.  */

template <class T>
f_type
missing_nothing2;

template <>
void *
ATTR ((alloc_size (1)))
missing_nothing2<char>(int);
