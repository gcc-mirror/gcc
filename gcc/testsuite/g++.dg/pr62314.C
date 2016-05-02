// { dg-options "-fdiagnostics-show-caret" }

template <typename T>
struct iterator_traits {};

struct file_iterator;

struct iterator_traits<file_iterator> { // { dg-error "explicit specialization must be preceded by .template" }
};

/* Verify that we emit a fixit hint for this case.  */

/* { dg-begin-multiline-output "" }
 struct iterator_traits<file_iterator>
 ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 template <> 
   { dg-end-multiline-output "" } */
