/* A replacement for Digital Unix's <va_list.h>.  */

#include <va-alpha.h>

#if !defined(_VA_LIST) && !defined(_HIDDEN_VA_LIST)
#define _VA_LIST
typedef __gnuc_va_list va_list;

#elif defined(_HIDDEN_VA_LIST) && !defined(_HIDDEN_VA_LIST_DONE)
#define _HIDDEN_VA_LIST_DONE
typedef __gnuc_va_list __va_list;

#elif defined(_HIDDEN_VA_LIST) && defined(_VA_LIST)
#undef _HIDDEN_VA_LIST

#endif
