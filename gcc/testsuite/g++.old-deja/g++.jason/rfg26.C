// { dg-do assemble  }
typedef void (FTYPE) ();
 
FTYPE f;                /* ok */
 
void
test_0 ()
{
    (FTYPE) f;          /* { dg-error "" } casting to function type */
}
