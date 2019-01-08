// { dg-additional-options "-fmodules-ts" }
module;
import Kevin;

export module Bob; // { dg-error "module already imported" }
// { dg-message "imported here" "Kevin.nms:" { target *-*-* } 0 }
