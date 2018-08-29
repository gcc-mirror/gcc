/* Example of a one-line C-style comment.  */
#if 0
{ dg-message "1: got C-style comment; length=45" "" { target *-*-* } .-2 }
{ dg-begin-multiline-output "" }
stripped content of comment: > Example of a one-line C-style comment.  <
{ dg-end-multiline-output "" }
#endif

     /*Another example of a one-line C-style comment.*/
#if 0
{ dg-message "6: got C-style comment; length=50" "" { target *-*-* } .-2 }
{ dg-begin-multiline-output "" }
stripped content of comment: >Another example of a one-line C-style comment.<
{ dg-end-multiline-output "" }
#endif

/**/
#if 0
{ dg-message "1: got C-style comment; length=4" "" { target *-*-* } .-2 }
{ dg-begin-multiline-output "" }
stripped content of comment: ><
{ dg-end-multiline-output "" }
#endif

/* Example of a
   multi-line C-style comment.  */
#if 0
{ dg-message "1: got C-style comment; length=50" "" { target *-*-* } .-3 }
{ dg-begin-multiline-output "" }
stripped content of comment: > Example of a
   multi-line C-style comment.  <
{ dg-end-multiline-output "" }
#endif

// Example of a C++-style comment
#if 0
{ dg-message "1: got C\\+\\+-style comment; length=33" "" { target *-*-* } .-2 }
{ dg-begin-multiline-output "" }
stripped content of comment: > Example of a C++-style comment<
{ dg-end-multiline-output "" }
#endif

//
#if 0
{ dg-message "1: got C\\+\\+-style comment; length=2" "" { target *-*-* } .-2 }
{ dg-begin-multiline-output "" }
stripped content of comment: ><
{ dg-end-multiline-output "" }
#endif
