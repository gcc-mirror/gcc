// Build don't link:

typedef void func_type ();
func_type *fp;
void *vp;

void example ()
{
    vp != fp;			// ERROR - no conversion from pfn to void*
}
