// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wcomments" }

/* /* */  // { dg-warning "\"\.\*\" within comment .-Wcomment." }

// \
          // { dg-warning "multi-line comment .-Wcomment." "multi-line" { target *-*-* } 6 }
