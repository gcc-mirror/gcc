// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Tudor Hulubei <tudor.hulubei@ecora.com>

// gcc 2.95 reports:
// invalid operands `foo' and `int' to binary `operator !='

class foo {} bar;
int i = void(bar) ? 1 : 0; // gets bogus error - operator!= - XFAIL *-*-*
