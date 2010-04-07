// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=trigraphs" }

??=  // { dg-error "trigraph \\?\\?= ignored, use -trigraphs to enable .-Wtrigraphs." }
