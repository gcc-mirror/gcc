typedef void (FTYPE) ();
 
FTYPE f;                /* ok */
 
void
test_0 ()
{
    (FTYPE) f;          /* ERROR - casting to function type */
}
