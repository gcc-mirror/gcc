fn main() {
    let tuple = (((),()),);

    // Do not reformat, the space after the second dot is required
    let _ = tuple.0. 1;
}
