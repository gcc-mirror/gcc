/* This file would generate an error because of #include_next, but the
   #pragma marks it a system header, so the error is suppressed.  */

#pragma GCC system_header
#include_next <stdio.h>
