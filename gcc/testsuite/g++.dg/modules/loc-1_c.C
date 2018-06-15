// { dg-additional-options "-fmodules-atom" }
import bob;
import stuart;

void kevin ()
{
  frob (nullptr); // { dg-error "call of overload" }
}

/* Sadly we can't specify a file name.  */
// { dg-message "candidate" "" { target *-*-* } 5 }
// { dg-message "candidate" "" { target *-*-* } 6 }
