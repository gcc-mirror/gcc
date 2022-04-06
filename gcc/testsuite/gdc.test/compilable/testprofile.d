// PERMUTE_ARGS:
// REQUIRED_ARGS: -profile

template LaLa(E...) {
        class LaLa {
                this() {

                }
        }
}

void main() {
        // doesn't work
        new LaLa!("lala", "lalalalala", "lala",
                "lala", "lala", "lala", "lalalala",
                "lala", "lala", "lala", "lalala",
                "lala", "lala", "lala", "lala",
                "lala", "lala", "lala", "lala",
                "lala", "lala", "lala", "lala",
                "lala", "lala", "lala", "lala");

}
