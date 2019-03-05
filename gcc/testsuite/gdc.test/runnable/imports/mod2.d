// mod2.d

module imports.mod2;

import mod1;

extern(C) int printf(const char*, ...);

void greet()
{
 printf(name().ptr);
}
