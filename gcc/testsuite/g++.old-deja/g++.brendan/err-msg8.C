// { dg-do assemble  }
// GROUPS passed error-messages
operator int ; int j; // { dg-error "" }  declaration of `operator int' as non-function.*
