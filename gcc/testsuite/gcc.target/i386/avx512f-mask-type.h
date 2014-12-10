/* Type of mask.  */
#if SIZE <= 8
#undef MASK_TYPE
#undef MASK_VALUE
#undef MASK_ALL_ONES
#define MASK_TYPE __mmask8
#define MASK_VALUE 0xB9
#define MASK_ALL_ONES 0xFF
#elif SIZE <= 16
#undef MASK_TYPE
#undef MASK_VALUE
#undef MASK_ALL_ONES
#define MASK_TYPE __mmask16
#define MASK_VALUE 0xA6BA
#define MASK_ALL_ONES 0xFFFF
#elif SIZE <= 32
#undef MASK_TYPE
#undef MASK_VALUE
#undef MASK_ALL_ONES
#define MASK_TYPE __mmask32
#define MASK_VALUE 0xA6BAAB6A 
#define MASK_ALL_ONES 0xFFFFFFFFu
#elif SIZE <= 64 
#undef MASK_TYPE
#undef MASK_VALUE
#undef MASK_ALL_ONES
#define MASK_TYPE __mmask64
#define MASK_VALUE 0xA6BAA6BAB6AB6ABB 
#define MASK_ALL_ONES 0xFFFFFFFFFFFFFFFFull
#endif
