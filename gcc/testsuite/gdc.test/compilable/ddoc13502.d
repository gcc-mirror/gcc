// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -w -o-
/*
TEST_OUTPUT:
---
compilable/ddoc13502.d(14): Warning: Ddoc: Stray '('. This may cause incorrect Ddoc output. Use $(LPAREN) instead for unpaired left parentheses.
compilable/ddoc13502.d(17): Warning: Ddoc: Stray '('. This may cause incorrect Ddoc output. Use $(LPAREN) instead for unpaired left parentheses.
compilable/ddoc13502.d(21): Warning: Ddoc: Stray '('. This may cause incorrect Ddoc output. Use $(LPAREN) instead for unpaired left parentheses.
compilable/ddoc13502.d(24): Warning: Ddoc: Stray '('. This may cause incorrect Ddoc output. Use $(LPAREN) instead for unpaired left parentheses.
---
*/

/// (
enum isSomeString(T) = true;

/// (
enum bool isArray(T) = true;


/// (
extern(C) alias int T1;

/// (
extern(C) alias T2 = int;
