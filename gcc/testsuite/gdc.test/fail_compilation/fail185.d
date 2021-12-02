/*
TEST_OUTPUT:
---
fail_compilation/fail185.d(10): Error: static assert:  "An error message
	that spans multiple lines, and also contains such characters as a tab,
\ and "."
---
*/

static assert (false,
"An error message
\tthat spans multiple lines, and also contains such characters as a tab,
\\ and \".");
