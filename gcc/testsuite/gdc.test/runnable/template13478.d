// EXTRA_FILES: imports/template13478a.d imports/template13478b.d

/// Tests emission of templates also referenced in speculative contexts.
/// Failure triggered with -inline.
module template13478;

import imports.template13478a;
import imports.template13478b;

int main() {
   return foo!int();
}
