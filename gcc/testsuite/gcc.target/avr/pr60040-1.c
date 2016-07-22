/* { dg-do compile } */
/* { dg-options "-Os" } */

float dhistory[10];
float test;

float getSlope(float history[]) {
  float sumx = 0;
  float sumy = 0;
  float sumxy = 0;
  float sumxsq = 0;
  float rate = 0;
  int n = 10;

  int i;
  for (i=1; i< 11; i++) {
    sumx = sumx + i;
    sumy = sumy + history[i-1];
    sumy = sumy + history[i-1];
    sumxsq = sumxsq + (i*i);
  }

  rate = sumy+sumx+sumxsq;
  return rate;
}

void loop() {
  test = getSlope(dhistory);
}
