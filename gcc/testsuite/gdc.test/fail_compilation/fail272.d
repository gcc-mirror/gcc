/*
TEST_OUTPUT:
---
fail_compilation/fail272.d(9): Error: circular reference to variable 'fail272.Ins!(Ins).Ins'
fail_compilation/fail272.d(10): Error: template instance fail272.Ins!(Ins) error instantiating
---
*/

template Ins(alias x) { const Ins = Ins!(Ins); }
alias Ins!(Ins) x;
