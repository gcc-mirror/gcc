// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

/***
 * See:
 *    http://www.fooa.com/test1
 *    http://www.fooa.com/_test1
 *    https://www.foob.com/test1
 *    $(LINK http://www.fooc.com/test1)
 *    $(LINK2 http://www.food.com/test1, test1)
 */

/**
Also piggyback a few tests for https://github.com/dlang/dmd/pull/6989

not_a_tag_because_it_does_not_start_with_uppercase:

not_a_tag_because_no_whitespace_after_colon:x

TagGalore: yes this is a tag

MoreTag:
yes the above is also a tag
*/

module test1;

int a;
