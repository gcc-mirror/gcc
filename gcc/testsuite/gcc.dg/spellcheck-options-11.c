/* Verify that we provide a hint if the user misspells an option argument
   (PR driver/69265).  */

/* { dg-do compile } */
/* { dg-options "-ftls-model=global-dinamic" } */
/* { dg-error "unknown TLS model 'global-dinamic'"  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments to '-ftls-model=' are: global-dynamic initial-exec local-dynamic local-exec; did you mean 'global-dynamic'?"  "" { target *-*-* } 0 } */
