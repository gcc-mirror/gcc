/* Test attributes for interrupt handlers */
/* { dg-do assemble } */
/* { dg-options "-mips32r2 -msoft-float" } */

void f () { }

void __attribute__ ((interrupt)) v0 () { }
void __attribute__ ((interrupt, use_shadow_register_set)) v1 () { }
void __attribute__ ((interrupt, keep_interrupts_masked)) v2 () { }
void __attribute__ ((interrupt, use_debug_exception_return)) v3 () { }
void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked)) v4 () { }
void __attribute__ ((interrupt, use_shadow_register_set, use_debug_exception_return)) v5 () { }
void __attribute__ ((interrupt, keep_interrupts_masked, use_debug_exception_return)) v6 () { }
void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked, use_debug_exception_return)) v7 () { }

void __attribute__ ((interrupt)) w0 () { t(); }
void __attribute__ ((interrupt, use_shadow_register_set)) w1 () { t(); }
void __attribute__ ((interrupt, keep_interrupts_masked)) w2 () { t(); }
void __attribute__ ((interrupt, use_debug_exception_return)) w3 () { t(); }
void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked)) w4 () { t(); }
void __attribute__ ((interrupt, use_shadow_register_set, use_debug_exception_return)) w5 () { t(); }
void __attribute__ ((interrupt, keep_interrupts_masked, use_debug_exception_return)) w6 () { t(); }
void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked, use_debug_exception_return)) w7 () { t(); }
