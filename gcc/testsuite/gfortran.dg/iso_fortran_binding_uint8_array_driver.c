#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <ISO_Fortran_binding.h>

extern void fsub(CFI_cdesc_t *);

int main(void)
{
   int8_t x[] = {1,2,3,4};
   int N = sizeof(x)/sizeof(x[0]);

   CFI_CDESC_T(1) dat;
   CFI_index_t ext[1];
   ext[0] = (CFI_index_t)N;
   int rc = CFI_establish((CFI_cdesc_t *)&dat, &x, CFI_attribute_other,
                      CFI_type_int8_t, 0, (CFI_rank_t)1, ext);
   printf("CFI_establish call returned: %d\n", rc);

   fsub((CFI_cdesc_t *)&dat );

   for (int i=0; i<N; i++)
       printf("%"PRId8"\n", x[i]);
   return 0;
}
