pub struct ReadDir {
    pub inner: i32,
    #[cfg(not(A))]
    pub end_of_stream: bool,
    #[cfg(A)]
    pub end_of_stream_but_different: bool,
}

fn main() {
    // Success
    let _ = ReadDir {
        inner: 14,
        #[cfg(not(A))]
        end_of_stream: false,
        #[cfg(A)]
        end_of_stream_but_different: false,
    };

    // Error
    let _ = ReadDir {
        // { dg-error "unknown field .end_of_stream_but_different. .E0560." "" { target *-*-* } .-1 }
        inner: 14,
        end_of_stream: false,
        end_of_stream_but_different: false,
    };
}
