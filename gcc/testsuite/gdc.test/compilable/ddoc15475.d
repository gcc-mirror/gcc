// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 15475

/**
My module
----
   // Computes the interval [x,y)
   auto interval = computeInterval(x, y);
----
*/
module ddoc15475;
