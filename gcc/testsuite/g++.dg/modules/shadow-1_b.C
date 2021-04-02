// { dg-additional-options -fmodules-ts }
import shadow;

// unfortunately not the exact same diagnostic in both cases :(

void stat (); // { dg-error "conflicts with import" }

struct stat {}; // { dg-error "in a different module" }
