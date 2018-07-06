// PR c++/84724
// { dg-do compile }
// { dg-options "" }

int __builtin_trap ();		// { dg-error "ambiguates built-in declaration" }
