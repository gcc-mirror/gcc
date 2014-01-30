/* Type of mask.  */
#if SIZE <= 8
#define MASK_TYPE __mmask8
#define MASK_VALUE 0xB9
#define MASK_ALL_ONES 0xFF
#elif SIZE <= 16
#define MASK_TYPE __mmask16
#define MASK_VALUE 0xA6BA
#define MASK_ALL_ONES 0xFFFF
#endif
