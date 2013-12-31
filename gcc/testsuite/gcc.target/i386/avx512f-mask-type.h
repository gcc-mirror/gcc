/* Type of mask.  */
#if SIZE <= 8
#define MASK_TYPE __mmask8
#define MASK_VALUE 0xB9
#elif SIZE <= 16
#define MASK_TYPE __mmask16
#define MASK_VALUE 0xA6BA
#endif
