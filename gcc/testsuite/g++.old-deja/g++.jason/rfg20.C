// { dg-do assemble  }

typedef void func_type ();
func_type *fp;
void *vp;

void example ()
{
    vp != fp;			// { dg-error "" } no conversion from pfn to void*
}
