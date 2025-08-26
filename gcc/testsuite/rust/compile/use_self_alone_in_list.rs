struct B;

use {B as B2, self};
// { dg-error ".self. import can only appear in an import list with a non-empty prefix" "" { target *-*-* } .-1 }

use {self};
// { dg-error ".self. import can only appear in an import list with a non-empty prefix" "" { target *-*-* } .-1 }
