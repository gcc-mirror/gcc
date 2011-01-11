#ifndef _OBJC_TEST_SUITE_NEXT_ENCODE_ASSIST_H_
#define _OBJC_TEST_SUITE_NEXT_ENCODE_ASSIST_H_

#ifdef __NEXT_RUNTIME__

#include "next-abi.h"
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
#include <objc/runtime.h>
typedef void * PMETH;
#else
#include <objc/objc-runtime.h>
typedef struct objc_method * PMETH;

/* Missing from old NeXT objc headers... */
#define _C_LNG_LNG  'q'
#define _C_ULNG_LNG 'Q'
#define _C_ATOM     '%'
#define _C_BOOL     'B'

#endif

/* The NeXT headers do not define NULL.  */
#ifndef NULL
#define NULL 0
#endif

#define _C_CONST        'r'
#define _C_IN           'n'
#define _C_INOUT        'N'
#define _C_OUT          'o'
#define _C_BYCOPY       'O'
#define _C_BYREF        'R'
#define _C_ONEWAY       'V'
#define _C_GCINVISIBLE  '!'
   
#define _F_CONST        0x01
#define _F_IN           0x01
#define _F_OUT          0x02
#define _F_INOUT        0x03
#define _F_BYCOPY       0x04  
#define _F_BYREF        0x08  
#define _F_ONEWAY       0x10
#define _F_GCINVISIBLE  0x20

/* The NeXT runtimes do not include these functions (at least not through 
   any public API).  They are required for the objc/execute/bf-* and bycopy-3. */

struct objc_struct_layout
{
  const char *original_type;
  const char *type;
  const char *prev_type;
  unsigned int record_size; 
  unsigned int record_align;
};

typedef union arglist {
  char *arg_ptr;
  char arg_regs[sizeof (char*)];
} *arglist_t;                   /* argument frame */

void objc_layout_structure_get_info (struct objc_struct_layout *,unsigned int *,
				     unsigned int *, const char **);
void objc_layout_structure (const char *, struct objc_struct_layout *);
BOOL objc_layout_structure_next_member (struct objc_struct_layout *);
void objc_layout_finish_structure (struct objc_struct_layout *, unsigned int *,
				   unsigned int *);

int objc_sizeof_type (const char *);
int objc_alignof_type (const char *);
int objc_aligned_size (const char *);
int objc_promoted_size (const char *);

unsigned objc_get_type_qualifiers (const char *);
const char *objc_skip_type_qualifiers (const char *);
const char *objc_skip_typespec (const char *);
const char *objc_skip_offset (const char *);
const char *objc_skip_argspec (const char *);

int method_get_number_of_arguments (PMETH);
int method_get_sizeof_arguments (PMETH);
char *method_get_next_argument (arglist_t , const char **);
char *method_get_first_argument (PMETH, arglist_t, const char **);
char *method_get_nth_argument (PMETH, arglist_t, int, const char **);

#endif /* __NEXT_RUNTIME__ */
#endif /* _OBJC_TEST_SUITE_NEXT_ENCODE_ASSIST_H_ */
