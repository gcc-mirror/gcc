// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 14413

module ddoc14413;

/// This should
/// be one
/// paragraph.
///
/// Paragraph 2
void foo(){}
