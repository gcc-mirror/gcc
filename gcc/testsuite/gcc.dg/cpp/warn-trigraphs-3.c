// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -trigraphs -Werror=trigraphs" }

??=  // { dg-error "trigraph \\?\\?= converted to # .-Wtrigraphs." }
