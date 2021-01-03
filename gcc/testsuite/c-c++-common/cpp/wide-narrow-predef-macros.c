/*
   { dg-do compile }
 */

#if !defined(__GNUC_EXECUTION_CHARSET_NAME)
#error "Required implementation macro for comple-time charset name is not present"
#endif
#if !defined(__GNUC_WIDE_EXECUTION_CHARSET_NAME)
#error "Required implementation macro for wide comple-time charset name is not present"
#endif

const char narrow_name[] = __GNUC_EXECUTION_CHARSET_NAME;
const char wide_name[] = __GNUC_WIDE_EXECUTION_CHARSET_NAME;
