// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Ulf Larsson <ulf.larsson@mbow337.swipnet.se>

template <class T> class C {};
class foo {} bar = bar.C(); // { dg-error "" } call to template
