// { dg-do assemble  }
typedef void (FTYPE) ();
 
FTYPE f;                /* ok */
 
void
test_0 ()
{
    (FTYPE) f;          /* { dg-error "5:invalid cast to function type" } casting to function type */
}
