/* Test attributes for interrupt handlers */
/* { dg-do assemble } */
/* { dg-options "-mips32r2 -msoft-float" } */

void f () { }

NOMIPS16 void __attribute__ ((interrupt)) v0 () { }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set)) v1 () { }
NOMIPS16 void __attribute__ ((interrupt, keep_interrupts_masked)) v2 () { }
NOMIPS16 void __attribute__ ((interrupt, use_debug_exception_return)) v3 () { }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked)) v4 () { }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set, use_debug_exception_return)) v5 () { }
NOMIPS16 void __attribute__ ((interrupt, keep_interrupts_masked, use_debug_exception_return)) v6 () { }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked, use_debug_exception_return)) v7 () { }

NOMIPS16 void __attribute__ ((interrupt)) w0 () { t(); }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set)) w1 () { t(); }
NOMIPS16 void __attribute__ ((interrupt, keep_interrupts_masked)) w2 () { t(); }
NOMIPS16 void __attribute__ ((interrupt, use_debug_exception_return)) w3 () { t(); }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked)) w4 () { t(); }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set, use_debug_exception_return)) w5 () { t(); }
NOMIPS16 void __attribute__ ((interrupt, keep_interrupts_masked, use_debug_exception_return)) w6 () { t(); }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set, keep_interrupts_masked, use_debug_exception_return)) w7 () { t(); }
