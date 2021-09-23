// { dg-additional-options "-fmodules-ts" }

module edith;

void gru ()
{
  me (1);
  you (1);
}

// { dg-regexp "\[^\n]*macloc-1_c.C:7:6: error: too many arguments to function 'int me@agnes\\(\\)'\nIn module agnes, imported at \[^\n]*macloc-1_b.C:8,\nof module edith, imported at \[^\n]*macloc-1_c.C:3:\n\[^\n]*macloc-1_a.C:11:12: note: declared here\n\[^\n]*macloc-1_a.C:8:20: note: in definition of macro 'BOB'\n" }

// { dg-regexp "\[^\n]*macloc-1_c.C:8:7: error: too many arguments to function 'int you@agnes\\(\\)'\nIn module agnes, imported at \[^\n]*macloc-1_b.C:8,\nof module edith, imported at \[^\n]*macloc-1_c.C:3:\n\[^\n]*macloc-1_a.C:12:14: note: declared here\n\[^\n]*macloc-1_a.C:9:22: note: in definition of macro 'KEVIN'\n" }
