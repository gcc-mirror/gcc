// Build don't link:

typedef void func_type ();
func_type *fp;
void *vp;

void example ()
{
    vp != fp;			// gets bogus error - nuttin' wrong wit dat
}
