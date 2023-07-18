// P2621R2 - UB? In My Lexer?
// { dg-do compile }

// [lex.phases] If a U+0027 APOSTROPHE or a U+0022 QUOTATION
// MARK matches the last category, the program is ill-formed.
const char * foo=" // { dg-error "terminating|expected" }
