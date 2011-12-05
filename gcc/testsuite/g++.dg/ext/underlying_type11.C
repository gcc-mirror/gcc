// PR c++/51414

__underlying_type(int[1]) i; // { dg-error "int \\\[1\\\]|invalid" }
