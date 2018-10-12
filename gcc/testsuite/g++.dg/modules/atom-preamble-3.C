// { dg-additional-options -fmodules-atom }

#define import import
// { dg-error "must be within module preamble" "" { target *-*-* } .-1 }
import malcolm; 
