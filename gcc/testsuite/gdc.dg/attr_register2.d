// { dg-do compile  { target x86_64-*-* } }

import gcc.attributes;

@register("ebx") static int var1 = void; // { dg-error "explicit register variable .var1. declared thread local" }

@register("ebx") extern int var2; // { dg-error "explicit register variable .var2. declared .extern." }

@register("ebp") __gshared int var3 = 0x2a; // { dg-error "global register variable has initial value" }

@register("ebp") __gshared int[256] var4 = void; // { dg-error "data type of .var4. isn.t suitable for a register" }
