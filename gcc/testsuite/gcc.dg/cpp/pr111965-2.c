/* PR preprocessor/111965
   { dg-do preprocess }
   { dg-options "-fdebug-cpp -fno-debug-cpp" }
   { dg-final { scan-file-not pr111965-2.i "P:<built-in>;F:<NULL>;" } } */
int x;
