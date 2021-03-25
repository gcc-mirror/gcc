// { dg-additional-options "-fmodules-ts" }

import edith;
import agnes;

void margo ()
{
  me (1);
  gru (2);
}

// { dg-regexp "\[^\n]*macloc-1_d.C:8:6: error: too many arguments to function 'int me@agnes\\(\\)'\nIn module agnes, imported at \[^\n]*macloc-1_d.C:4:\n\[^\n]*macloc-1_a.C:11:12: note: declared here\n\[^\n]*macloc-1_a.C:8:20: note: in definition of macro 'BOB'\n" }
// { dg-regexp "\[^\n]*macloc-1_d.C:9:7: error: too many arguments to function 'void gru@edith\\(\\)'\nIn module edith, imported at \[^\n]*macloc-1_d.C:3:\n\[^\n]*macloc-1_b.C:10:20: note: declared here\n\[^\n]*macloc-1_b.C:6:19: note: in definition of macro 'STUART'\n" }
