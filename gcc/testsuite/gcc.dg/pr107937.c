/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

int _setjmp(int);
int regs, vm_debug_engine_vp_0, vm_debug_engine_vp_2;

void
vm_dispatch_hook();


void
vm_debug_engine() {
  int fp;
  void *jump_table = &&l_nop;
l_nop:
  if (__builtin_expect(vm_debug_engine_vp_2, 0))
    vm_dispatch_hook();
  if (_setjmp(regs)) {
    fp = fp;
    vm_dispatch_hook();
    goto *jump_table;
  }
  vm_debug_engine_vp_0 = fp;
}
