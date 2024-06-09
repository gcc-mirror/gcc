/* PR preprocessor/111965
   { dg-do preprocess }
   { dg-options "-fdebug-cpp" }
   { dg-final { scan-file pr111965-1.i "P:<built-in>;F:<NULL>;" } } */
int x;
