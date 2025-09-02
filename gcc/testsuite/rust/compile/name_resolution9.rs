pub mod foo {
    pub mod bar {
        fn f() {
            super::super::super::foo!(); // { dg-error "too many leading .super. keywords" }
                                         // { dg-error "could not resolve macro invocation" "" { target *-*-* } .-1 }

            super::crate::foo!(); // { dg-error ".crate. in paths can only be used" }
                                  // { dg-error "could not resolve macro invocation" "" { target *-*-* } .-1 }

			
            crate::foo::bar::super::foo!(); // { dg-error ".super. in paths can only be used" }
		                                    // { dg-error "could not resolve macro invocation" "" { target *-*-* } .-1 }
        }
    }
}
