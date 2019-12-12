// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 7795

module ddoc7795;

struct TimeValue {
    this(int hour, int minute, int second = 0, int ms = 0) {}
}

///
struct DateTime {
    ///
    this(int x, TimeValue t = TimeValue(0, 0)) {}
}

void main() { }
