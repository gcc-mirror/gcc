// PR c++/114229
// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-module-cmi modB }

export module modB;
import modA;
