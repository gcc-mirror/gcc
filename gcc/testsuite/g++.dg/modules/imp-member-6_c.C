// PR c++/124478
// { dg-additional-options "-fmodules -fdump-lang-module-alias" }

import tools;
import dedupe;
DedupeFilesPath analyse(int) {
  return {};
}

// Find the implicit member from dedupe when attempting to instantiate it ourselves.
// { dg-final { scan-lang-dump {Adding implicit member '::DedupeFilesPath@tools:.::__dt @dedupe:.'} module } }
