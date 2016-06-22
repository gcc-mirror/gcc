/* { dg-options "-fdiagnostics-show-caret -Wno-deprecated" } */
#import <this-file-does-not-exist.h> /* { dg-error "9: this-file-does-not-exist.h: No such file or directory" } */

/* { dg-begin-multiline-output "" }
 #import <this-file-does-not-exist.h>
         ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
compilation terminated.
   { dg-end-multiline-output "" } */
