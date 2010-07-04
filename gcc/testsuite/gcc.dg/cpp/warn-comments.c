// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wcomments" }

/* /* */  // { dg-warning "\"\.\*\" within comment .-Wcomments." }

// \
          // { dg-warning "multi-line comment .-Wcomments." "multi-line" { target *-*-* } 6 }
