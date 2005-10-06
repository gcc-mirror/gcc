// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }

void run (void) {
  float stack[1];
  float *sp = stack;
  try
  {
    float value2 = ((float) *(--sp));
    float value1 = ((float) *(--sp));
    *(sp++) = (value1 - value2);
  }
  catch (int *ex)
  {
  }
}
