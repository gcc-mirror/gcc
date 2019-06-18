// PERMUTE_ARGS: -w
// REQUIRED_ARGS: -o- -D -Dd${RESULTS_DIR}/compilable
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc13270;

/**
 * My overloaded function.
 *
 * Params:
 *      task = String description of stuff to do.
 *      tasks = Array of descriptions of stuff to do.
 *      maxJobs = Max parallel jobs to run while doing stuff.
 */
void doStuff(string task) {}

/// ditto
void doStuff(string[] tasks, int maxJobs) {}
