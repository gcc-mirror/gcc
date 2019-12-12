// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 14778

module ddoc14778;

/// docs for Z
template Z14778(T)
{
    /// docs for E
    enum E;

    /// docs for x
    enum x = 1.0;

    /// docs for mv
    auto mv = 1;

    /// docs for wv
    inout wv = 3;

    /// doc for cv
    const cv = "a";

    /// docs for wcv
    inout const wcv = "ab";

    /// doc for sv
    shared sv = 1.4142;

    /// doc for swv
    shared inout swv = 3.14;

    /// doc for scv
    shared const scv = new Object();

    /// docs for swcv
    shared inout const swcv = undefined;

    /// doc for iv
    immutable iv = [1,2,3];
}
