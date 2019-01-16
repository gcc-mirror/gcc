// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 2

/**
 * Summary
 *
 * Description1
 *
 * Description2
 *
 * Description3
 *
 * Macros:
 *	WIKI = StdStream
 *	meemie
 * See_Also:
 *	Things to see also.
 *
 *	And more things.
 */

/*
 */

module std.test;

/// A base class for stream exceptions.
class StreamException: Exception {
  /** Construct a StreamException with given error message msg.
   * Params:
   *	msg = the $(RED red) $(BLUE blue) $(GREEN green) $(YELLOW yellow).
   *	foo = next parameter which is a much longer
   *		message spanning multiple
   *		lines.
   */
  this(string msg, int foo) { super(msg); }

    /********** stars ***************/
    int stars;
}

