// { dg-options "-g" }
// { dg-do compile }

// On some platforms like MIPS, __builtin_va_list is a
// RECORD_TYPE. Make sure we don't wrongly try to generate debug info
// for its TYPE_DECL and crash.
typedef __builtin_va_list foo;
