

import edith;
import agnes;

void margo ()
{
  me (1);
  gru (2);
}

// { dg-regexp "\[^\n]*macloc-1_d.C:8:8: error: too many arguments to function 'int me\\(\\)'\nIn module agnes, imported at \[^\n]*macloc-1_d.C:4:\n\[^\n]*macloc-1_a.C:11:12: note: declared here\n\[^\n]*macloc-1_a.C:8:20: note: in definition of macro 'BOB'\n" }
// { dg-regexp "\[^\n]*macloc-1_d.C:9:9: error: too many arguments to function 'void gru\\(\\)'\nIn module agnes, imported at \[^\n]*macloc-1_d.C:4:\n\[^\n]*macloc-1_a.C:12:8: note: declared here\n\[^\n]*macloc-1_a.C:9:24: note: in definition of macro 'KEVIN'\n" }
