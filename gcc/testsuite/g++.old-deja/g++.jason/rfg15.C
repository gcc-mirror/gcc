const void *cvp;
 
const void func1 ()
{
        return *cvp;    /* ERROR - returning a value from a void function */
}
