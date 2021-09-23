// { dg-do compile { target x86_64-*-* } }

import gcc.attributes;

// target

@target_clones("default")
@target("default")
void tc0(); // { dg-warning "ignoring attribute .target. because it conflicts with attribute .target_clones." }

// target_clones

@target("default")
@target_clones("default")
void t0(); // { dg-warning "ignoring attribute .target_clones. because it conflicts with attribute .target." }

@always_inline
@target_clones("default")
void tc1(); // { dg-warning "ignoring attribute .target_clones. because it conflicts with attribute .always_inline." }
