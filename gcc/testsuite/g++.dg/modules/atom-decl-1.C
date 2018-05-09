// { dg-options "-fmodules-atom" }

int i; // { dg-message "ended here" }

module thing; // { dg-error "within module preamble" }
