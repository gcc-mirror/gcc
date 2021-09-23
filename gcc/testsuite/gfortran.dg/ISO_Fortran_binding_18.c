#include <stdlib.h>
#include <string.h>
#include <ISO_Fortran_binding.h>


extern int do_loop(CFI_cdesc_t* array);

int main(int argc, char ** argv)
{
	int nx = 9;
	int ny = 10;
	int nz = 2;

	int arr[nx*ny*nz];
	memset(arr,0,sizeof(int)*nx*ny*nz);
	CFI_index_t shape[3];
	shape[0] = nz;
	shape[1] = ny;
	shape[2] = nx;

	CFI_CDESC_T(3) farr;
	int rc = CFI_establish((CFI_cdesc_t*)&farr, arr, CFI_attribute_other, CFI_type_int, 0, (CFI_rank_t)3, (const CFI_index_t *)shape);
	if (rc != CFI_SUCCESS) abort();
	int result = do_loop((CFI_cdesc_t*)&farr);
	if (result != nx*ny*nz) abort();
	return 0;
}
