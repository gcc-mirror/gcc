#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)		\
  do								\
    {								\
      __emit_string_literal_range ("*.%s%u", 2, 2, 3); \
    }								\
  while (0)
