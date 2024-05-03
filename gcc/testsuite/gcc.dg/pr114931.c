/* { dg-do compile } */
/* { dg-options "-std=c23" } */

struct Tcl_Obj;
void(Tcl_FreeInternalRepProc)(struct Tcl_Obj *);
typedef struct Tcl_Obj {
} Tcl_Obj;
struct {
  void (*tclFreeObj)(Tcl_Obj *);
} Tcl_InitStubs;
