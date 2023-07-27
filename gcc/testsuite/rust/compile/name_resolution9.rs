// { dg-options "-frust-name-resolution-2.0" }

pub mod foo {
    pub mod bar {
        fn f() {
            super::super::super::foo!(); // { dg-error "too many leading .super. keywords" }
                                         // { dg-error "could not resolve macro invocation" "" { target *-*-* } .-1 }

            super::crate::foo!(); // { dg-error "leading path segment .crate. can only be used" }
                                  // { dg-error "could not resolve macro invocation" "" { target *-*-* } .-1 }

			
            crate::foo::bar::super::foo!(); // { dg-error "leading path segment .super. can only be used" }
		                                    // { dg-error "could not resolve macro invocation" "" { target *-*-* } .-1 }
        }
    }
}
