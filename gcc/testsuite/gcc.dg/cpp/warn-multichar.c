// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wmultichar" }

#if 'abc'  // { dg-warning "multi-character character constant .-Wmultichar." }
#endif
