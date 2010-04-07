// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=comments" }

/* /* */  // { dg-error "\"\.\*\" within comment .-Wcomments." }

// \
          // { dg-error "multi-line comment .-Wcomments." "multi-line" { target *-*-* } 6 }
