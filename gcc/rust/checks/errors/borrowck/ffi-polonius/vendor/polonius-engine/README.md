This is a core library that models the borrow check. It implements the
analysis [described in this blogpost][post]. This library is intended
for use both by rustc and by the polonius crate, which is a distinct
front-end intended for testing, profiling, etc.

[post]: http://smallcultfollowing.com/babysteps/blog/2018/04/27/an-alias-based-formulation-of-the-borrow-checker/
