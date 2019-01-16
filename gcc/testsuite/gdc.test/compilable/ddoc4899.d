// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -wi -o-

/*
TEST_OUTPUT:
---
compilable/ddoc4899.d(18): Warning: Ddoc: Stray '('. This may cause incorrect Ddoc output. Use $(LPAREN) instead for unpaired left parentheses.
compilable/ddoc4899.d(19): Warning: Ddoc: Stray ')'. This may cause incorrect Ddoc output. Use $(RPAREN) instead for unpaired right parentheses.
---
*/

/++
       (See accompanying file LICENSE_1_0.txt or copy at
        foo:)
+/
module d;

/** ( */ int a;
/** ) */ int b;

void main()
{
}
