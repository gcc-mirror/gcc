// { dg-do assemble  }

typedef void func_type ();
func_type *fp;
void *vp;

void example ()
{
    vp != fp;			// { dg-error "forbids comparison" } no conversion from pfn to void*
}
