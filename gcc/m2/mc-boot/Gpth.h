

#if !defined (_pth_H)
#   define _pth_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_pth_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

typedef struct pth_proc_p pth_proc;

typedef unsigned int pth_size_t;

typedef void *pth_pth_uctx_t;

typedef void (*pth_proc_t) (void *);
struct pth_proc_p { pth_proc_t proc; };

EXTERN int pth_pth_select (int p1, void * p2, void * p3, void * p4, void * p5);
EXTERN int pth_pth_uctx_create (void * p);
EXTERN int pth_pth_uctx_make (pth_pth_uctx_t p1, void * p2, pth_size_t p3, void * p4, pth_proc p5, void * p6, pth_pth_uctx_t p7);
EXTERN int pth_pth_uctx_save (pth_pth_uctx_t p1);
EXTERN int pth_pth_uctx_switch (pth_pth_uctx_t p1, pth_pth_uctx_t p2);
EXTERN int pth_pth_init (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
