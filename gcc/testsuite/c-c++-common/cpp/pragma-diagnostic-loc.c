/* { dg-do preprocess } */
/* PR preprocessor/114423 */
/* Check that we now issue diagnostics at the location of the _Pragma
   instead of an invalid location.  If we someday manage to issue
   diagnostics at better locations in the future, this will need
   updating.  */
_Pragma("GCC warning \"warning1\"") /* { dg-warning "1:warning1" } */
#define P _Pragma("GCC warning \"warning2\"") /* { dg-warning "11:warning2" } */
P /* { dg-note "in expansion of macro" } */
#define S "GCC warning \"warning3\""
/**/ _Pragma(S) /* { dg-warning "6:warning3" } */

/* This diagnostic uses a different code path (cpp_diagnostic_at() rather
   than cpp_error_with_line()).  Also make sure that the dg-note location
   does not get overridden to the _Pragma location.  */
#pragma GCC poison xyz /* { dg-note "poisoned here" } */
/* */ _Pragma("xyz") /* { dg-error "7:attempt to use poisoned" } */
