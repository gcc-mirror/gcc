/* { dg-do compile } */
/* { dg-options "-mcmse" }  */

#include <arm_cmse.h>

void foo()
{
	int *data;
	cmse_check_address_range((int*)data, 0, 0); /* { dg-warning "ignoring return value" } */
}
