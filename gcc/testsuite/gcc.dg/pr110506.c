/* { dg-do compile } */
/* { dg-options "-O2" } */

struct {
  long *sp;
  long *csp;
} neko_interp_loop_vm;
int neko_interp_loop_vm_2;
void neko_interp_loop()
{
  void *pc[] = {&&LabelAccGlobal, &&LabelPhysCompare, &&LabelTailCall,
      &&LabelLoop, &&LabelMakeArray2};
  long *sp, *csp = neko_interp_loop_vm.csp;
LabelAccGlobal:
  neko_interp_loop_vm.sp = sp;
  neko_interp_loop_vm.csp = csp;
  goto * 0;
LabelTailCall:
  csp = sp -= neko_interp_loop_vm_2;
LabelMakeArray2:
LabelPhysCompare:
LabelLoop:
  goto * 0;
}
