// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=multichar" }

#if 'abc'  // { dg-error "multi-character character constant .-Wmultichar." }
#endif
