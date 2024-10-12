// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -trigraphs -Werror=trigraphs" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
??=  // { dg-error "trigraph '\\?\\?=' converted to '#' .-Werror=trigraphs." }
