// PR c++/83871 - wrong code for attribute const and pure on distinct
// template specializations
// Test to verify that a declaration of an explicit specialization with
// no attributes is diagnosed when the primary template is declared with
// one or more attributes.  The warning helps highlight a change in GCC
// 8 from previous versions that copied the attributes from the primary
// to the specialization.  It also helps point out simply forgetting to
// declare the specialization with an attribute.
// { dg-do compile }
// { dg-options "-Wmissing-attributes" }

#define ATTR(list)   __attribute__ (list)


// Verify that a primary without attributes doesn't cause warnings.
template <class T> void fnoattr ();

template <> void fnoattr<void>();
template <> void ATTR ((cold)) fnoattr<int>();
template <> void ATTR ((hot)) fnoattr<double>();

// Verify that a noreturn primary also doesn't cause warnings.
template <class T> int ATTR ((noreturn)) fnoreturn ();

template <> int fnoreturn<void>();
template <> int ATTR ((cold)) fnoreturn<int>();
template <> int ATTR ((hot)) fnoreturn<double>();


template <class T>
void*
ATTR ((malloc, alloc_size (1)))
missing_all (int);            // { dg-message "missing primary template attributes \(.malloc., .alloc_size.|.alloc_size., .malloc.\)" }

template <>
void*
missing_all<char>(int);       // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }

// Verify that specifying the same attributes in whatever order
// doesn't trigger the warning, even when other attributes are
// added.
template <>
void*
ATTR ((alloc_size (1), malloc))
missing_all<char>(int);

template <>
void*
ATTR ((alloc_size (1))) ATTR ((malloc)) ATTR ((returns_nonnull))
missing_all<char>(int);   // T = char, same as above

template <>
void*
ATTR ((hot)) ATTR ((alloc_size (1))) ATTR ((malloc))
missing_all<char>(int);   // T = char, same as above

// Verify that the following attributes suppress the warning.
template <> void* ATTR ((error (""))) missing_all<short>(int);
template <> void* ATTR ((deprecated)) missing_all<int>(int);
template <> void* ATTR ((warning (""))) missing_all<double>(int);


template <class T>
void*
ATTR ((malloc, alloc_size (1)))
missing_malloc (int);             // { dg-message "missing primary template attribute .malloc." }

template <>
void*
ATTR ((alloc_size (1)))
missing_malloc<char>(int);            // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }

template <> void* ATTR ((malloc, alloc_size (1))) missing_malloc<short>(int);
template <> void* ATTR ((deprecated)) missing_malloc<int>(int);
template <> void* ATTR ((error (""))) missing_malloc<long>(int);
template <> void* ATTR ((warning (""))) missing_malloc<double>(int);

template <class T>
void*
ATTR ((malloc, alloc_size (1)))
missing_alloc_size (int, int);        // { dg-message "missing primary template attribute .alloc_size." }

template <>
void*
ATTR ((malloc))
missing_alloc_size<char>(int, int);   // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }


template <class T>
void*
ATTR ((nonnull (1)))
missing_nonnull (void*);              // { dg-message "missing primary template attribute .nonnull." }

template <>
void*
ATTR ((malloc))
missing_nonnull<char>(void*);         // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }

template <> void* ATTR ((nonnull (1))) missing_nonnull<short>(void*);
template <> void* ATTR ((deprecated)) missing_nonnull<int>(void*);
template <> void* ATTR ((error (""))) missing_nonnull<long>(void*);
template <> void* ATTR ((warning (""))) missing_nonnull<double>(void*);
