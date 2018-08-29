// PR target/84272
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-march=armv8-a -mtune=cortex-a57" { target aarch64-*-* } }

struct A
{
  float b, c;
  A ();
  A (float, float, float);
  float operator * (A)
  {
    float d = b * b + c * c;
    return d;
  }
};

void
foo ()
{
  A g[1];
  A h (0, 0, h * g[2]);
}
