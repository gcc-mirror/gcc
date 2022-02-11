// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

/**
 * Module docs.
 */
module ddoc14383;

/// Ddoc'd unittest
unittest
{
    int iShouldAppearInTheDocs;
}
