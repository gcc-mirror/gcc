// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 11823
module ddoc11823;

/// file function name is _file, arg defaults to __FILE__ but not __something__
void file(string arg) { }
