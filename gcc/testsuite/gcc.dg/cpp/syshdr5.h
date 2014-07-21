/* Origin: PR preprocessor/60723

   This header file is to be included by the syshdr5.c file.  */

#pragma GCC system_header
#define FOO(A)do {int line = __LINE__ ; A;} while(0)
