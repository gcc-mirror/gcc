// PR c++/24302
// { dg-options "-Wunused" }

static union
{
  unsigned char FLT4ARR[4]; 
  float FLT4;
}; // { dg-warning "used" }
