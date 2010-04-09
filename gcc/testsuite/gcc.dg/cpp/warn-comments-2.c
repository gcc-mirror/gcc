// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=comments" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
/* /* */  // { dg-error "\"\.\*\" within comment .-Wcomments." }

// \
          // { dg-error "multi-line comment .-Wcomments." "multi-line" { target *-*-* } 6 }
