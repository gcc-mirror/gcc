#if __x86_64__ || __i386__
register int i asm("esp");
#else
int i;
#endif
