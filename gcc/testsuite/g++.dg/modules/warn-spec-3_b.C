// { dg-additional-options "-fmodules -fdump-lang-module" }

// Test that we don't re-export the changes from M.
// { dg-final { scan-lang-dump {Diagnostic changes: 0} module } }

export module N;
import M;
