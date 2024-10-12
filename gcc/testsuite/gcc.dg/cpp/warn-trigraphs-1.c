// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -trigraphs -Wtrigraphs" }

??=  // { dg-warning "trigraph '\\?\\?=' converted to '#' .-Wtrigraphs." }
