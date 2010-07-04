// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=comments" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
/* /* */  // { dg-error "\"\.\*\" within comment .-Werror=comments." }

// \
          // { dg-error "multi-line comment .-Werror=comments." "multi-line" { target *-*-* } 6 }
