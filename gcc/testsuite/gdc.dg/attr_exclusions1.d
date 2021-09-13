// { dg-do compile }

import gcc.attributes;

// always_inline

@noinline
@always_inline
void i0(); // { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .noinline." }

@target_clones("")
@always_inline
void i1(); // { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .target_clones." }

// noinline

@always_inline
@noinline
void n0(); // { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." }
