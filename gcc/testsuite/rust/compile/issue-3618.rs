#![feature(no_core)]
#![no_core]

static _X : ()
	    = loop{}; // { dg-error "'loop' is not allowed in const context" }
