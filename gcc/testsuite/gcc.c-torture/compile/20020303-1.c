/* With -fzero-initialized-in-bss, we made I a common symbol instead
   of a symbol in the .bss section.  Not only does that break semantics,
   but a common symbol can't be weak.  */

int i __attribute__((weak)) = 0;
