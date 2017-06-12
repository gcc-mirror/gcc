// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wcomments" }

/* /* */  // { dg-warning "4: \"\.\*\" within comment .-Wcomment." }

// \
          // { dg-warning "1: multi-line comment .-Wcomment." "multi-line" { target *-*-* } .-1 }
