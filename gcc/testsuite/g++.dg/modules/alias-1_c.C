// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/alias-1.map" }
// { dg-module-bmi bob }

export module bob;
import "alias-1_a.H";
