// { dg-additional-options "-fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/alias-1.map" }
// { dg-module-bmi kevin }

export module kevin;
import <alias-1_a.H>;
