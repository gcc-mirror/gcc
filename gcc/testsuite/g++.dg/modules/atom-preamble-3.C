// { dg-additional-options -fmodules-atom }

#define import import  // { dg-message "ended here" }
// { dg-error "must be within module preamble" "" { target *-*-* } .-1 }
import malcolm; 
