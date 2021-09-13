// EXTRA_SOURCES: extra-files/ddoc3.ddoc
// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

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
 *	ARG0 = $0
 *	ARG1 = $1
 *	ARG2 = $2
 *	ARG3 = $3
 *	PLUS = $+
 *	TROW = $(TR $(TCOL $1,$+))
 *	TCOL = $(TD $1) $(TCOL $+)
 *	LPAREN = (
 * See_Also:
 *	Things to see also.
 *
 *	And more things $(BR)
 *	'arg1, arg2, arg3' : $(ARG0 arg1, arg2, arg3). $(BR)
 *	'arg2, arg3' : $(PLUS arg1, arg2, arg3). $(BR)
 *	'arg1' : $(ARG1 arg1, arg2, arg3). $(BR)
 *	'arg2' : $(ARG2 arg1, arg2, arg3). $(BR)
 *	'arg3' : $(ARG3 arg1, arg2, arg3). $(BR)
 */

/**
 *	Things to see also $(HELLO).
 *
 *	$(TABLE
 *	$(TROW 1, 2, 3)
 *	$(TROW 4, 5, 6)
 *	)
 *
 * $(D_CODE 
      $(B pragma)( $(I name) );
      $(B pragma)( $(I name) , $(I option) [ $(I option) ] );
      $(U $(LPAREN))
  )
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

