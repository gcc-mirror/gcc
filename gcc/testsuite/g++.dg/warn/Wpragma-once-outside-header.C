// { dg-do assemble  }
// { dg-options "-Werror=pragma-once-outside-header" }
// { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 }

#pragma once  // { dg-error "'#pragma once' in main file" }
int main() {}
