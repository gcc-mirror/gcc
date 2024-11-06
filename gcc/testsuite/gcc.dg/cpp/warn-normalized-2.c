// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wnormalized=nfkc" }

\u00AA  // { dg-warning "'.U000000aa' is not in NFKC .-Wnormalized=." }
