/* { dg-do link } */
/* { dg-additional-options "-std=gnu99" } */

#ifdef __FLASH

long val;

__attribute__((used))
const __flash long*
load4_flash (const __flash long *p)
{
    val += *p++;
    val += *p++;
    return p;
}

#endif

int main (void)
{
	return 0;
}
