extern void _text;
/* We use __SIZE_TYPE__ here because it's as wide as a pointer, so we
   know we won't have a non-constant because of extension or
   truncation of the pointer to fit.  */
static __SIZE_TYPE__ x
  = (__SIZE_TYPE__) &_text - (__SIZE_TYPE__) 0x10000000L - 1;
