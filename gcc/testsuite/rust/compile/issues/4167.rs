#![feature(no_core)]
#![no_core]
macro_rules! the_macro {
    ( $foo:stmt ; $bar:stmt ; ) => {
        #[cfg(foo)]
        $foo

        $foo[cfg(bar)]
        $bar
    };
}

fn the_function() {
    the_macro!( (); (); ); // { dg-error "cannot strip expression in this position" }
}
