/* Contributed by Nicholas Ormrod
   Origin: PR preprocessor/60723.

   This file is to be included by the syshdr4.c file.  */

#pragma GCC system_header

#define FOO() int line = __LINE__ ;
