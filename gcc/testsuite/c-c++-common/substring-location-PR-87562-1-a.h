#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)		\
  do								\
    {								\
      __builtin_sprintf (LABEL, "*.%s%u", PREFIX, (unsigned) (NUM));	\
    }								\
  while (0)
