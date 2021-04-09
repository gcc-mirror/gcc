// PR 99071 ICE with global-module merging
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename T>
void begin (T *);
