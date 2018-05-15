// { dg-do preprocess }
// { dg-additional-options -EE }

// no imports, so no problem.
#if 1
int i;
#endif

// { dg-final { scan-file-not cpp-preamble-7.i "int" } }
