// { dg-additional-options "-w" }

mod orange {
    fn tangerine() {}

    mod green {
        mod blue {
            fn berry() {
                tangerine();
            }
        }
    }
}
