// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh
// REQUIRED_ARGS: -d

// Test notes: 'main' is the symbol being documented (DDOC_AUTO_PSYMBOL),
// 'arguments' is a parameter (DDOC_AUTO_PARAM), and 'false' is a keyword
// (DDOC_AUTO_KEYWORD).
/**
 * The main thing this program does is nothing, and I do _not want to hear any
 * false arguments about that!
 *
 * Macros:
 * DDOC_AUTO_PSYMBOL = $0
 * DDOC_AUTO_KEYWORD = $0
 * DDOC_AUTO_PARAM = $0
 * DDOC_AUTO_PSYMBOL_SUPPRESS = HALPIMBEINGSUPPRESSED $0
 *
 * DDOC = $(BODY)
 * DDOC_DECL = $0
 * DDOC_MEMBER_HEADER =
 * DDOC_MODULE_MEMBERS = $0
 * DDOC_MEMBER = $0
 */
void main(string[] arguments)
{
}
