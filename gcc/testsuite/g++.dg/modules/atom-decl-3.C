// { dg-options "-fmodules-atom" }

int i; // { dg_message "ended here" }
import baz; // { dg-error "must be within module preamble" }

